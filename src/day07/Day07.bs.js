// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Solution = require("../Solution.bs.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Caml_splice_call = require("rescript/lib/js/caml_splice_call.js");

function make(input) {
  var numbers = input.split(",").map(Belt_Int.fromString).map(Belt_Option.getExn);
  var max = Caml_splice_call.spliceApply(Math.max, [numbers]);
  var match = Belt_Array.makeBy(max - 1 | 0, (function (i) {
            return i + 1 | 0;
          })).reduce((function (param, pos) {
          var bestPosFuelUsed = param[1];
          var fuel = numbers.reduce((function (acc, i) {
                  return acc + Math.abs(i - pos | 0) | 0;
                }), 0);
          if (bestPosFuelUsed !== undefined && fuel >= bestPosFuelUsed) {
            return [
                    param[0],
                    bestPosFuelUsed
                  ];
          } else {
            return [
                    pos,
                    fuel
                  ];
          }
        }), [
        undefined,
        undefined
      ]);
  console.log("best pos is", match[0]);
  console.log("fuel used", match[1]);
  
}

var Part01 = {
  make: make
};

function fuelUse(dist) {
  return dist / 2 * (1 + dist);
}

function make$1(input) {
  var numbers = input.split(",").map(Belt_Int.fromString).map(Belt_Option.getExn);
  var max = Caml_splice_call.spliceApply(Math.max, [numbers]);
  var match = Belt_Array.makeBy(max, (function (i) {
            return i + 1 | 0;
          })).reduce((function (param, pos) {
          var bestPosFuelUsed = param[1];
          var fuel = numbers.reduce((function (acc, i) {
                  var fuelUsed = fuelUse(Math.abs(i - pos));
                  return acc + (fuelUsed | 0) | 0;
                }), 0);
          if (bestPosFuelUsed !== undefined && fuel >= bestPosFuelUsed) {
            return [
                    param[0],
                    bestPosFuelUsed
                  ];
          } else {
            return [
                    pos,
                    fuel
                  ];
          }
        }), [
        undefined,
        undefined
      ]);
  console.log("best pos is", match[0]);
  console.log("fuel used", match[1]);
  
}

var Part02 = {
  fuelUse: fuelUse,
  make: make$1
};

Solution.make(make, "day07/input");

Solution.make(make$1, "day07/input");

exports.Part01 = Part01;
exports.Part02 = Part02;
/*  Not a pure module */