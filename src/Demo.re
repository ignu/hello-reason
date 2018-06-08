open Js.Option;

type suit =
  | Hearts
  | Diamonds
  | Clubs
  | Spades;
type value =
  | Ace
  | King
  | Queen
  | Jack
  | Num(int);
type card =
  | Card(suit, value)
  | Joker;

let suitToString = s =>
  switch (s) {
  | Hearts => "Hearts"
  | Diamonds => "Diamonds"
  | Clubs => "Clubs"
  | Spades => "Spades"
  };

let numToString = num =>
  switch (num) {
  | 1 => "One"
  | 2 => "Two"
  | 3 => "Three"
  | 4 => "Four"
  | 5 => "Five"
  | 6 => "Six"
  | 7 => "Seven"
  | 8 => "Eight"
  | 9 => "Nine"
  | 10 => "Ten"
  | _ => failwith("Not a valid card number")
  };

let valueToString = value =>
  switch (value) {
  | Ace => "Ace"
  | King => "King"
  | Queen => "Queen"
  | Jack => "Jack"
  | Num(n) => numToString(n)
  };

let renderCard = card =>
  switch (card) {
  | Joker => "Joker"
  | Card(suit, value) =>
    valueToString(value) ++ " of " ++ suitToString(suit)
  };

let parseSuit = suitStr =>
  switch (suitStr) {
  | "H" => Some(Hearts)
  | "D" => Some(Diamonds)
  | "C" => Some(Clubs)
  | "S" => Some(Spades)
  | _ => None
  };

let parseNumber = number => {
  let parsed =
    try (number |> int_of_string |> some) {
    | Failure(_) => None
    };

  switch (parsed) {
  | Some(n) when n >= 2 && n <= 10 => Some(Num(n))
  | _ => None
  };
};

let parsePlainCard = card => {
  let length = Js.String.length(card);
  let suit = card |> Js.String.sliceToEnd(~from=length - 1) |> parseSuit;
  let value =
    card |> Js.String.slice(~from=0, ~to_=length - 1) |> parseNumber;

  switch (suit, value) {
  | (Some(suit), Some(value)) => Some(Card(suit, value))
  | _ => None
  };
};
let parseCard = card =>
  switch (card) {
  | "J" => Some(Joker)
  | str => parsePlainCard(str)
  };

let optionMap = (fn, opt) =>
  switch (opt) {
  | Some(x) => fn(x) |> some
  | None => None
  };

let optionWithDefault = (defaultVal, opt) =>
  switch (opt) {
  | Some(x) => x
  | None => defaultVal
  };

let printCard = card =>
  card
  |> parseCard
  |> optionMap(renderCard)
  |> optionWithDefault("-- invalid card --")
  |> Js.log;

"8H" |> printCard;
"10H" |> printCard;
"J" |> printCard;