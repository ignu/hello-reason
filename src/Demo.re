type suit = Hearts | Diamonds | Clubs | Spades;
type value = Ace | King | Queen | Jack | Num(int);
type card = Card(suit, value);

let parseAndRenderSuit = suit => {
  switch suit {
    | "H" => Some("Hearts")
    | "D" => Some("Diamonds")
    | "C" => Some("Clubs")
    | "S" => Some("Spades")
    | _ => None
  }
};
let suiteToString
let parseAndRenderValue = value => {
  switch value {
    | "1" => Some("One")
    | "2" => Some("Two")
    | "3" => Some("Three")
    | "4" => Some("Four")
    | "5" => Some("Five")
    | "10" => Some("Ten")
    | "A" => Some("Ace")
    | "K" => Some("King")
    | _ => None
  }
};

let parseAndRenderCard = card => {
  let length = Js.String.length(card);
  let suit = Js.String.sliceToEnd(~from=length - 1, card)
    |> parseAndRenderSuit;
  let value = Js.String.slice(~from=0, ~to_=length - 1, card)
    |> parseAndRenderValue;

    switch(value, suit) {
    | (Some(value), Some(suit)) => value ++ " of " ++ suit
    |  _ => "Unknown Card"
    };
};

"2H" |> parseAndRenderCard |> Js.log;
"10H" |> parseAndRenderCard |> Js.log;
"KC" |> parseAndRenderCard |> Js.log;

