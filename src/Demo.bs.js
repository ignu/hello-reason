// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';


function parseAndRenderSuit(suit) {
  switch (suit) {
    case "C" : 
        return /* Some */["Clubs"];
    case "D" : 
        return /* Some */["Diamonds"];
    case "H" : 
        return /* Some */["Hearts"];
    case "S" : 
        return /* Some */["Spades"];
    default:
      return /* None */0;
  }
}

function parseAndRenderValue(value) {
  switch (value) {
    case "1" : 
        return /* Some */["One"];
    case "10" : 
        return /* Some */["Ten"];
    case "2" : 
        return /* Some */["Two"];
    case "3" : 
        return /* Some */["Three"];
    case "4" : 
        return /* Some */["Four"];
    case "5" : 
        return /* Some */["Five"];
    case "A" : 
        return /* Some */["Ace"];
    case "K" : 
        return /* Some */["King"];
    default:
      return /* None */0;
  }
}

function parseAndRenderCard(card) {
  var length = card.length;
  var suit = parseAndRenderSuit(card.slice(length - 1 | 0));
  var value = parseAndRenderValue(card.slice(0, length - 1 | 0));
  if (value && suit) {
    return value[0] + (" of " + suit[0]);
  } else {
    return "Unknown Card";
  }
}

console.log(parseAndRenderCard("2H"));

console.log(parseAndRenderCard("10H"));

console.log(parseAndRenderCard("KC"));

exports.parseAndRenderSuit = parseAndRenderSuit;
exports.parseAndRenderValue = parseAndRenderValue;
exports.parseAndRenderCard = parseAndRenderCard;
/*  Not a pure module */