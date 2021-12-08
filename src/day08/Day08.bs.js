// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Solution = require("../Solution.bs.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Caml_array = require("rescript/lib/js/caml_array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");

function make(input) {
  return input.split("\n").map(function (line) {
                return line.split(" | ")[1].split(" ").map(function (x) {
                              return x.split("").length;
                            }).filter(function (x) {
                            if (x >= 5) {
                              return x === 7;
                            } else {
                              return x >= 2;
                            }
                          }).length;
              }).reduce((function (acc, i) {
                return acc + i | 0;
              }), 0);
}

var Part01 = {
  make: make
};

function includes(partialOpt, str1, str2) {
  var partial = partialOpt !== undefined ? partialOpt : 0;
  if (str2.length > 0) {
    if (partial > 0) {
      return str2.filter(function ($$char) {
                  return str1.indexOf($$char) > -1;
                }).length === partial;
    } else {
      return str2.every(function ($$char) {
                  return str1.indexOf($$char) > -1;
                });
    }
  } else {
    return false;
  }
}

function uniqStrToDigit(str) {
  var match = str.length;
  switch (match) {
    case 2 :
        return 1;
    case 3 :
        return 7;
    case 4 :
        return 4;
    case 5 :
    case 6 :
        return -1;
    case 7 :
        return 8;
    default:
      return -1;
  }
}

function strToDigit(str, found) {
  var match = str.length;
  switch (match) {
    case 2 :
        return 1;
    case 3 :
        return 7;
    case 4 :
        return 4;
    case 5 :
        if (includes(undefined, str, Caml_array.get(found, 7))) {
          return 3;
        } else if (includes(3, str, Caml_array.get(found, 4))) {
          return 5;
        } else if (includes(2, str, Caml_array.get(found, 4))) {
          return 2;
        } else {
          return -1;
        }
    case 6 :
        if (includes(undefined, str, Caml_array.get(found, 4))) {
          return 9;
        } else if (includes(undefined, str, Caml_array.get(found, 7))) {
          return 0;
        } else if (includes(undefined, str, Caml_array.get(found, 7))) {
          return -1;
        } else {
          return 6;
        }
    case 7 :
        return 8;
    default:
      return -1;
  }
}

function make$1(input) {
  return input.split("\n").map(function (line) {
                var digits = line.replace(" | ", " ").split(" ");
                var found = digits.reduce((function (acc, x) {
                        var chars = x.split("");
                        var num = uniqStrToDigit(chars);
                        if (num > -1) {
                          Caml_array.set(acc, num, chars);
                        }
                        return acc;
                      }), Belt_Array.makeBy(10, (function (param) {
                            return [];
                          })));
                var jooh = digits.reduce((function (acc, x) {
                        var chars = x.split("");
                        var num = strToDigit(chars, found);
                        if (num > -1) {
                          Caml_array.set(acc, num, chars);
                        } else {
                          console.log("unresolved", chars);
                        }
                        return acc;
                      }), found);
                var output = line.split(" | ")[1].split(" ").map(function (x) {
                      var chars = x.split("");
                      return jooh.findIndex(function (foundChars) {
                                  if (foundChars.length === chars.length) {
                                    return includes(undefined, foundChars, chars);
                                  } else {
                                    return false;
                                  }
                                });
                    });
                return Belt_Option.getExn(Belt_Int.fromString(output.join("")));
              }).reduce((function (acc, i) {
                return acc + i | 0;
              }), 0);
}

var Part02 = {
  includes: includes,
  uniqStrToDigit: uniqStrToDigit,
  strToDigit: strToDigit,
  make: make$1
};

Solution.make(make, "day08/input");

Solution.make(make$1, "day08/input");

exports.Part01 = Part01;
exports.Part02 = Part02;
/*  Not a pure module */
