use iced::{
    button, image::Handle as ImageHandle, pick_list, Align, Application, Button, Checkbox, Column,
    Command, Element as IcedElement, HorizontalAlignment, Image, Length, PickList, Row, Settings,
    Text,
};
use image::io::Reader as ImageReader;
use itertools::Itertools;
use minidom::{quick_xml::Reader, Element, NSChoice};
use once_cell::sync::Lazy;
use regex::Regex;
use thiserror::Error;

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fs::{create_dir_all, File};
use std::io::BufReader;
use std::path::PathBuf;

static FORBIDDEN_RE: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"addchest|coins_x(?:15|2)|resource_(?:coin(?:[0-9]|10)$|hoard_gold|diamond)|bomb_grenade|misc_magnet").unwrap()
});
static USE_TEXT_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"resource|double_heart_transplant|spell_charm").unwrap());

#[derive(Debug)]
struct Item {
    id: String,
    name: String,
    slot: Slot,
    image: ImageHandle,
}

impl Item {
    fn weapon_type(&self) -> WeaponType {
        for t in WeaponType::all() {
            if self.id.contains(&t.to_string()) {
                return t;
            }
        }
        WeaponType::Other
    }

    fn other_type(&self) -> OtherItemType {
        for t in OtherItemType::all() {
            if self.id.contains(&t.to_string()) {
                return t;
            }
        }
        OtherItemType::Other
    }
}

#[derive(Debug, Clone)]
struct Character {
    id: String,
    name: String,
    items: Vec<String>,
    curses: HashSet<Slot>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
enum Slot {
    Shovel,
    Weapon,
    Head,
    Feet,
    Body,
    Ring,
    Spell,
    Torch,
    Action,
    Misc,
    Other,
}

impl Slot {
    fn all() -> Vec<Self> {
        use Slot::*;
        vec![
            Shovel, Weapon, Body, Head, Feet, Torch, Ring, Spell, Action, Misc, Other,
        ]
    }
}

impl std::str::FromStr for Slot {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Slot::*;
        Ok(match s {
            "shovel" => Shovel,
            "weapon" => Weapon,
            "head" => Head,
            "feet" => Feet,
            "body" => Body,
            "ring" => Ring,
            "spell" => Spell,
            "torch" => Torch,
            "action" => Action,
            "misc" => Misc,
            "hud" | "bomb" => Other,
            _ => return Err(()),
        })
    }
}

impl fmt::Display for Slot {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Slot::*;
        write!(
            f,
            "{}",
            match self {
                Shovel => "shovel",
                Weapon => "weapon",
                Head => "head",
                Feet => "feet",
                Body => "body",
                Ring => "ring",
                Spell => "spell",
                Torch => "torch",
                Action => "action",
                Misc => "misc",
                Other => "other",
            }
        )
    }
}

#[derive(Debug)]
struct UI {
    root: Element,
    items: Vec<Item>,
    characters: Vec<Character>,
    current_character: usize,
    current_char_pick: pick_list::State<CharPick>,
    slots: Vec<SlotUI>,
    menu_choices: Option<Vec<button::State>>,
    menu_location: Option<MenuLocation>,
}

#[derive(Debug)]
struct SlotUI {
    slot: Slot,
    button: button::State,
    items: Option<Vec<button::State>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MenuLocation {
    Shovel,
    Weapon(Option<WeaponType>),
    Head,
    Feet,
    Body,
    Ring,
    Spell,
    Torch,
    Action,
    Misc,
    Other(Option<OtherItemType>),
}

impl MenuLocation {
    fn to_slot(&self) -> Slot {
        use MenuLocation::*;
        match self {
            Shovel => Slot::Shovel,
            Weapon(_) => Slot::Weapon,
            Head => Slot::Head,
            Feet => Slot::Feet,
            Body => Slot::Body,
            Ring => Slot::Ring,
            Spell => Slot::Spell,
            Torch => Slot::Torch,
            Action => Slot::Action,
            Misc => Slot::Misc,
            Other(_) => Slot::Other,
        }
    }

    fn from_slot(s: &Slot) -> Self {
        use MenuLocation::*;
        match s {
            Slot::Shovel => Shovel,
            Slot::Head => Head,
            Slot::Weapon => Weapon(None),
            Slot::Feet => Feet,
            Slot::Body => Body,
            Slot::Ring => Ring,
            Slot::Spell => Spell,
            Slot::Torch => Torch,
            Slot::Action => Action,
            Slot::Misc => Misc,
            Slot::Other => Other(None),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WeaponType {
    Dagger,
    Broadsword,
    Longsword,
    Whip,
    Spear,
    Rapier,
    Bow,
    Crossbow,
    Flail,
    Cat,
    Axe,
    Harp,
    Warhammer,
    Staff,
    Cutlass,
    Other,
}

impl WeaponType {
    fn all() -> Vec<Self> {
        use WeaponType::*;
        vec![
            Dagger, Broadsword, Longsword, Whip, Spear, Rapier, Crossbow, Bow, Flail, Cat, Axe,
            Harp, Warhammer, Staff, Cutlass, Other,
        ]
    }
}

impl fmt::Display for WeaponType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use WeaponType::*;
        write!(
            f,
            "{}",
            match self {
                Dagger => "dagger",
                Broadsword => "broadsword",
                Longsword => "longsword",
                Whip => "whip",
                Spear => "spear",
                Rapier => "rapier",
                Bow => "bow",
                Crossbow => "crossbow",
                Flail => "flail",
                Cat => "cat",
                Axe => "axe",
                Harp => "harp",
                Warhammer => "warhammer",
                Staff => "staff",
                Cutlass => "cutlass",
                Other => "other",
            }
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OtherItemType {
    Resource,
    Other,
}

impl OtherItemType {
    fn all() -> Vec<Self> {
        use OtherItemType::*;
        vec![Other, Resource]
    }
}

impl fmt::Display for OtherItemType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use OtherItemType::*;
        write!(
            f,
            "{}",
            match self {
                Resource => "resource",
                Other => "other",
            }
        )
    }
}

#[derive(Debug, Clone)]
enum Message {
    CharPicked(usize),
    SlotPressed(Slot),
    CurseSlot(Slot, bool),
    RemoveItem(usize, usize),
    ChooseItem(Slot, String),
    ChooseMenu(MenuLocation),
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct CharPick {
    idx: usize,
    name: String,
}

impl fmt::Display for CharPick {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl UI {
    fn write_xml(&mut self) {
        let new_chars = character_element(&self.characters[..]);
        self.root.remove_child("characters", NSChoice::None);
        self.root.append_child(new_chars);
        let result: Result<(), MainError> = File::create("mods/Unawareness/necrodancer.xml")
            .map_err(Into::into)
            .and_then(|mut f| Ok(self.root.write_to(&mut f)?));
        if let Err(e) = result {
            eprintln!("{}", e);
        }
    }
}

fn display_item<R>(i: &Item) -> IcedElement<'static, R> {
    if USE_TEXT_RE.is_match(i.id.as_str()) {
        Text::new(i.name.replace(" ", "\n"))
            .horizontal_alignment(HorizontalAlignment::Center)
            .into()
    } else {
        Image::new(i.image.clone())
            .width(50.into())
            .height(50.into())
            .into()
    }
}

impl Application for UI {
    type Executor = iced::executor::Default;
    type Message = Message;
    type Flags = NDXData;

    fn new(ndxdata: NDXData) -> (Self, Command<Message>) {
        let NDXData {
            root,
            items,
            characters,
        } = ndxdata;
        (
            Self {
                root,
                items,
                characters,
                current_character: 0,
                current_char_pick: Default::default(),
                slots: Slot::all()
                    .into_iter()
                    .map(|slot| SlotUI {
                        slot,
                        button: Default::default(),
                        items: None,
                    })
                    .collect(),
                menu_location: None,
                menu_choices: None,
            },
            Command::none(),
        )
    }

    fn title(&self) -> String {
        "Unawareness".to_string()
    }

    fn update(&mut self, message: Message) -> Command<Message> {
        use Message::*;
        match message {
            CharPicked(i) => {
                self.current_character = i;
                self.menu_location = None;
                self.menu_choices = None;
                self.slots = Slot::all()
                    .into_iter()
                    .map(|slot| SlotUI {
                        slot,
                        button: Default::default(),
                        items: None,
                    })
                    .collect();
                Command::none()
            }
            SlotPressed(s) => {
                let l = MenuLocation::from_slot(&s);
                if self.menu_location == Some(l) {
                    self.menu_location = None;
                } else {
                    self.menu_location = Some(l);
                }
                self.menu_choices = None;
                Command::none()
            }
            CurseSlot(s, b) => {
                if b && s != Slot::Other {
                    self.characters[self.current_character].curses.insert(s);
                } else {
                    self.characters[self.current_character].curses.remove(&s);
                }
                self.write_xml();
                Command::none()
            }
            RemoveItem(slot, item) => {
                self.characters[self.current_character].items.remove(item);
                self.slots[slot].items = None;
                self.write_xml();
                Command::none()
            }
            ChooseItem(slot, id) => {
                self.characters[self.current_character].items.push(id);
                self.slots
                    .iter_mut()
                    .find(|x| x.slot == slot)
                    .unwrap()
                    .items = None;
                self.write_xml();
                Command::none()
            }
            ChooseMenu(ml) => {
                self.menu_location = Some(ml);
                self.menu_choices = None;
                Command::none()
            }
        }
    }

    fn view(&mut self) -> IcedElement<Message> {
        let char = &self.characters[self.current_character];

        let char_picker = PickList::new(
            &mut self.current_char_pick,
            self.characters
                .iter()
                .enumerate()
                .map(|(idx, c)| CharPick {
                    idx,
                    name: c.name.clone(),
                })
                .collect::<Vec<_>>(),
            Some(CharPick {
                idx: self.current_character,
                name: char.name.clone(),
            }),
            |p| Message::CharPicked(p.idx),
        );

        let items_by_id: HashMap<String, &Item> =
            self.items.iter().map(|i| (i.id.clone(), i)).collect();
        let items_by_slot: HashMap<Slot, Vec<&Item>> =
            self.items.iter().map(|i| (i.slot, i)).into_group_map();

        let slots = self
            .slots
            .iter_mut()
            .enumerate()
            .map(
                |(
                    slot_idx,
                    SlotUI {
                        slot,
                        button,
                        items,
                    },
                )| {
                    let slot = *slot;
                    let slot_button = Button::new(button, Text::new(slot.to_string()))
                        .on_press(Message::SlotPressed(slot));
                    let cursed_checkbox =
                        Checkbox::new(char.curses.contains(&slot), "cursed", move |b| {
                            Message::CurseSlot(slot, b)
                        })
                        .spacing(5);
                    let items_in_slot: Vec<(usize, &Item)> = char
                        .items
                        .iter()
                        .enumerate()
                        .filter_map(|(n, i)| {
                            let x = items_by_id[i];
                            if x.slot == slot {
                                Some((n, x))
                            } else {
                                None
                            }
                        })
                        .collect();
                    let item_buttons = if let Some(items) = items {
                        items
                    } else {
                        let v: Vec<button::State> =
                            items_in_slot.iter().map(|_| Default::default()).collect();
                        items.replace(v);
                        items.as_mut().unwrap()
                    };
                    let n = items_in_slot.len();
                    let button_column = Column::with_children(
                        items_in_slot
                            .into_iter()
                            .zip(item_buttons)
                            .map(|((n, i), b)| {
                                Button::new(b, display_item(i))
                                    .on_press(Message::RemoveItem(slot_idx, n))
                                    .into()
                            })
                            .collect(),
                    )
                    .align_items(Align::Center)
                    .height(if n > 5 { Length::Shrink } else { 350.into() });
                    Column::new()
                        .align_items(Align::Center)
                        .spacing(5)
                        .push(slot_button)
                        .push(cursed_checkbox)
                        .push(button_column)
                        .into()
                },
            )
            .collect();

        fn items_to_menu_choices<'a>(
            slot: Slot,
            items: impl Iterator<Item = &'a Item>,
        ) -> Vec<(IcedElement<'static, Message>, Message)> {
            items
                .map(|i| (display_item(i), Message::ChooseItem(slot, i.id.clone())))
                .collect()
        }

        let menu = {
            let menu_choices = match &self.menu_location {
                None => vec![],
                Some(MenuLocation::Weapon(weapon_type)) => {
                    if let Some(weapon_type) = weapon_type {
                        let e = vec![];
                        let weapons = items_by_slot
                            .get(&Slot::Weapon)
                            .unwrap_or(&e)
                            .into_iter()
                            .filter(|w| w.weapon_type() == *weapon_type);
                        items_to_menu_choices(Slot::Weapon, weapons.cloned())
                    } else {
                        WeaponType::all()
                            .into_iter()
                            .map(|t| {
                                (
                                    Text::new(t.to_string()).into(),
                                    Message::ChooseMenu(MenuLocation::Weapon(Some(t))),
                                )
                            })
                            .collect()
                    }
                }
                Some(MenuLocation::Other(other_type)) => {
                    if let Some(other_type) = other_type {
                        let e = vec![];
                        let items = items_by_slot
                            .get(&Slot::Other)
                            .unwrap_or(&e)
                            .into_iter()
                            .filter(|x| x.other_type() == *other_type);
                        items_to_menu_choices(Slot::Other, items.cloned())
                    } else {
                        OtherItemType::all()
                            .into_iter()
                            .map(|t| {
                                (
                                    Text::new(t.to_string()).into(),
                                    Message::ChooseMenu(MenuLocation::Other(Some(t))),
                                )
                            })
                            .collect()
                    }
                }
                Some(slot) => {
                    let slot = slot.to_slot();
                    let e = vec![];
                    let slot_items = items_by_slot.get(&slot).unwrap_or(&e);
                    items_to_menu_choices(slot, slot_items.into_iter().cloned())
                }
            };

            if self.menu_choices.is_none() {
                let v: Vec<button::State> =
                    menu_choices.iter().map(|_| Default::default()).collect();
                self.menu_choices.replace(v);
            };
            let menu_choice_buttons = self.menu_choices.as_mut().unwrap();

            menu_choices
                .into_iter()
                .zip(menu_choice_buttons)
                .map(|((l, m), b)| Button::new(b, l).on_press(m).into())
                .collect()
        };

        Row::new()
            .push(char_picker)
            .push(
                Column::new()
                    .align_items(Align::Center)
                    .push(Row::with_children(slots).spacing(10))
                    .push(layout_rows(menu, 8)),
            )
            .into()
    }
}

fn layout_rows<'a, R>(v: Vec<IcedElement<'a, R>>, width: usize) -> IcedElement<R>
where
    R: 'a,
{
    Column::with_children(
        v.into_iter()
            .chunks(width)
            .into_iter()
            .map(|row| {
                Row::with_children(row.collect())
                    .align_items(Align::Center)
                    .into()
            })
            .collect(),
    )
    .align_items(Align::Center)
    .into()
}

struct NDXData {
    root: Element,
    items: Vec<Item>,
    characters: Vec<Character>,
}

fn load_necrodancer_xml() -> Result<NDXData, MainError> {
    let mut reader = Reader::from_reader(BufReader::new(
        File::open("mods/Unawareness/necrodancer.xml")
            .or_else(|_| File::open("data/necrodancer.xml"))?,
    ));
    let root = Element::from_reader(&mut reader)?;

    let flyaway_re = Regex::new(r"^\|[^\|]*\|([^\|]*)\|$").unwrap();
    let items_e = root
        .get_child("items", NSChoice::None)
        .ok_or_else(|| MainError::BadNecroXML("missing <items> tag".to_string()))?;
    let items: Vec<Item> = items_e
        .children()
        .map(|i| {
            let id = i.name().to_string();
            let flyaway = i.attr("flyaway").unwrap_or(&id[..]);
            let name = flyaway_re
                .captures(flyaway)
                .and_then(|c| c.get(1))
                .map(|m| m.as_str())
                .unwrap_or(flyaway)
                .to_string();
            let slot = if let Some(s) = i.attr("slot") {
                s.parse()
                    .map_err(|_| MainError::BadNecroXML(format!("bad item slot: {}", s)))?
            } else {
                Slot::Other
            };
            let image_path = i.text();
            let width = i
                .attr("imageW")
                .map(str::parse)
                .transpose()
                .ok()
                .flatten()
                .unwrap_or(24);
            let height = i
                .attr("imageH")
                .map(str::parse)
                .transpose()
                .ok()
                .flatten()
                .unwrap_or(24);
            let img = ImageReader::open(
                ["data/items/", image_path.as_str()]
                    .iter()
                    .collect::<PathBuf>(),
            )?
            .decode()?
            .crop(0, 0, width, height)
            .to_bgra8();
            let image = ImageHandle::from_pixels(img.width(), img.height(), img.into_vec());

            Ok(Item {
                id,
                name,
                slot,
                image,
            })
        })
        .filter(|i| {
            i.as_ref()
                .map(|i| !FORBIDDEN_RE.is_match(i.id.as_str()))
                .unwrap_or(true)
        })
        .collect::<Result<Vec<Item>, MainError>>()?;

    let characters_e = root
        .get_child("characters", NSChoice::None)
        .ok_or_else(|| MainError::BadNecroXML("missing <characters> tag".to_string()))?;
    let names: HashMap<&str, &str> = vec![
        ("0", "Cadence"),
        ("1", "Melody"),
        ("2", "Aria"),
        ("3", "Dorian"),
        ("4", "Eli"),
        ("5", "Monk"),
        ("6", "Dove"),
        ("7", "Coda"),
        ("8", "Bolt"),
        ("9", "Bard"),
        ("10", "Nocturna"),
        ("11", "Diamond"),
        ("12", "Mary"),
        ("13", "Tempo"),
        ("14", "Reaper"),
    ]
    .into_iter()
    .collect();
    let characters: Vec<Character> = characters_e
        .children()
        .map(|c| {
            if !c.is("character", NSChoice::None) {
                return Err(MainError::BadNecroXML("bad character tag".to_string()));
            }
            let id = c
                .attr("id")
                .ok_or_else(|| MainError::BadNecroXML("character missing id attr".to_string()))?
                .to_string();
            let name = names.get(&id[..]).unwrap_or(&&id[..]).to_string();
            let c = c
                .get_child("initial_equipment", NSChoice::None)
                .ok_or_else(|| MainError::BadNecroXML("missing <initial_equipment>".to_string()))?;
            let mut items = Vec::new();
            let mut curses = HashSet::new();
            for x in c.children() {
                if x.is("item", NSChoice::None) {
                    items.push(
                        x.attr("type")
                            .ok_or_else(|| {
                                MainError::BadNecroXML("item missing type attr".to_string())
                            })?
                            .to_string(),
                    );
                } else if x.is("cursed", NSChoice::None) {
                    curses.insert(
                        x.attr("slot")
                            .ok_or_else(|| {
                                MainError::BadNecroXML("cursed missing slot attr".to_string())
                            })?
                            .parse()
                            .map_err(|_| MainError::BadNecroXML("bad cursed slot".to_string()))?,
                    );
                } else {
                    return Err(MainError::BadNecroXML("bad initial equipment".to_string()));
                }
            }
            Ok(Character {
                id,
                name,
                items,
                curses,
            })
        })
        .collect::<Result<_, _>>()?;

    Ok(NDXData {
        root,
        items,
        characters,
    })
}

fn character_element(chars: &[Character]) -> Element {
    Element::builder("characters")
        .append_all(chars.iter().map(|c| {
            Element::builder("character").attr("id", &c.id[..]).append(
                Element::builder("initial_equipment")
                    .append_all(
                        c.items
                            .iter()
                            .map(|i| Element::builder("item").attr("type", i)),
                    )
                    .append_all(
                        c.curses
                            .iter()
                            .map(|c| Element::builder("cursed").attr("slot", c.to_string())),
                    ),
            )
        }))
        .build()
}

#[derive(Debug, Error)]
enum MainError {
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error(transparent)]
    Minidom(#[from] minidom::Error),
    #[error("Missing variable `{0}`")]
    DotEnv(String, #[source] dotenv::Error),
    #[error("Malformed necrodancer.xml: {0}")]
    BadNecroXML(String),
    #[error(transparent)]
    Iced(#[from] iced::Error),
    #[error(transparent)]
    Image(#[from] image::ImageError),
}

fn main_2() -> Result<(), MainError> {
    let necrodancer_path = dotenv::var("NECRODANCER_PATH")
        .map_err(|e| MainError::DotEnv("NECRODANCER_PATH".to_string(), e))?;
    std::env::set_current_dir(necrodancer_path)?;
    create_dir_all("mods/Unawareness")?;

    let ndxdata = load_necrodancer_xml()?;

    UI::run(Settings::with_flags(ndxdata))?;
    Ok(())
}

fn main() -> Result<(), MainError> {
    main_2().map_err(|e| {
        eprintln!("{}", e);
        e
    })
}
