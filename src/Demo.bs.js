// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Js_exn = require("bs-platform/lib/js/js_exn.js");
var Js_option = require("bs-platform/lib/js/js_option.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function suitToString(s) {
  switch (s) {
    case 0 : 
        return "Hearts";
    case 1 : 
        return "Diamonds";
    case 2 : 
        return "Clubs";
    case 3 : 
        return "Spades";
    
  }
}

function numToString(num) {
  var switcher = num - 1 | 0;
  if (switcher > 9 || switcher < 0) {
    return Pervasives.failwith("Not a valid card number");
  } else {
    switch (switcher) {
      case 0 : 
          return "One";
      case 1 : 
          return "Two";
      case 2 : 
          return "Three";
      case 3 : 
          return "Four";
      case 4 : 
          return "Five";
      case 5 : 
          return "Six";
      case 6 : 
          return "Seven";
      case 7 : 
          return "Eight";
      case 8 : 
          return "Nine";
      case 9 : 
          return "Ten";
      
    }
  }
}

function valueToString(value) {
  if (typeof value === "number") {
    switch (value) {
      case 0 : 
          return "Ace";
      case 1 : 
          return "King";
      case 2 : 
          return "Queen";
      case 3 : 
          return "Jack";
      
    }
  } else {
    return numToString(value[0]);
  }
}

function renderCard(card) {
  return valueToString(card[1]) + (" of " + suitToString(card[0]));
}

function parseSuit(suitStr) {
  switch (suitStr) {
    case "C" : 
        return /* Some */[/* Clubs */2];
    case "D" : 
        return /* Some */[/* Diamonds */1];
    case "H" : 
        return /* Some */[/* Hearts */0];
    case "S" : 
        return /* Some */[/* Spades */3];
    default:
      return /* None */0;
  }
}

function parseNumber(number) {
  var parsed;
  try {
    parsed = Js_option.some(Caml_format.caml_int_of_string(number));
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.failure) {
      parsed = /* None */0;
    } else {
      throw exn;
    }
  }
  if (parsed) {
    var n = parsed[0];
    if (n >= 2 && n <= 10) {
      return /* Some */[/* Num */[n]];
    } else {
      return /* None */0;
    }
  } else {
    return /* None */0;
  }
}

function parseCard(card) {
  var length = card.length;
  var suit = parseSuit(card.slice(length - 1 | 0));
  var value = parseNumber(card.slice(0, length - 1 | 0));
  if (suit && value) {
    return /* Some */[/* Card */[
              suit[0],
              value[0]
            ]];
  } else {
    return /* None */0;
  }
}

exports.suitToString = suitToString;
exports.numToString = numToString;
exports.valueToString = valueToString;
exports.renderCard = renderCard;
exports.parseSuit = parseSuit;
exports.parseNumber = parseNumber;
exports.parseCard = parseCard;
/* No side effect */
