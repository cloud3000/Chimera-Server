TABTABLE = [];
TABIDX = 0;
TABCNT = 0;
TAB = false;


alertBox = function getFocus() {
  document.getElementById("alertbox").style.display = "table";
}

function addTable(tbl) {
  var row = document.getElementById('r' + tbl.rownum);
  row.innerHTML = row.innerHTML +
    '';
}

function delRowitems(rnum) {
  var row = document.getElementById('r' + rnum);
  row.innerHTML = '';
}

function addFieldset(fset) {
  var row = document.getElementById('r' + fset.rownum);
  row.innerHTML = row.innerHTML +
    '<fieldset class="'+ fset.class +'" ' +
    'style="height:'+  fset.height +'vh; width:'+ fset.width +'vw; ' +
    'position:absolute;left: ' + fset.column + 'vw;">' +
    // '<legend>&nbsp'+ fset.title +'&nbsp</legend>' +
    '</fieldset>';
}

function addSpan(static) {
  var row = document.getElementById('r' + static.rownum);
  cls = mkclass(static.enh);
  row.innerHTML = row.innerHTML +
                  '<div class="static ' + static.align + ' ' + cls +
                  '" style="position:absolute;left: ' +
                  static.column + 'vw; width: ' + static.width + 'vw;"' +
                  '><p>' + static.value + '</p></div>'
}

function addInput(field) {
  var row = document.getElementById('r' + field.rownum);
  var cls = mkclass(field.enh);
  var datamax = parseInt(field.length) + 1;
  row.innerHTML = row.innerHTML +
             '<input id="' + 'i' + field.fieldnum.trim() + '" type="'+ field.element +'" name="' + field.name + '"' +
             ' title="' + field.title + '"'+
             ' onfocus="setTABIDX(this.id)" ' +
             ' onblur="blurFunction(this.id)" ' +
             ' onkeypress="keypressFunction(event, this.id)"' +
             ' onkeydown="keydownFunction(event, this.id)"' +
             ' class="' + cls + '"' +
             ' maxlength="' + datamax + '"' +
             ' value="' + field.value + '"' +
             ' style="position:absolute;left: ' + field.column + 'vw;' +
             ' width:' + field.length + 'vw;">';
}
function setFkey(fkey) {
  id1 = "f" + fkey.keynum + "a";
  id2 = "f" + fkey.keynum + "b";
  l1 = document.getElementById(id1)
  l1.innerHTML = fkey.value1;
  l2 = document.getElementById(id2)
  l2.innerHTML = fkey.value2;
}

function clearform() {
  // var head = document.getElementById('descr');
  // head.innerHTML = '';
  for (var i = 1; i < 25; i++) {
    delRowitems(i)
  }
} // house of reps, congress: 202-225-3121

function mkclass(enh) {
  var cls = '';
  if ( enh === "NONE") {
    return enh;
  }
  var list = enh.split("");
  list.sort();
  for (var i = 0; i < list.length; i++) {
    if (list[i] === "H") {
      cls = cls + "ehb"
    }
    if (list[i] === "I") {
      cls = cls + "ein"
    }
    if (list[i] === "U") {
      cls = cls + "eul"
    }
    if (list[i] === "B") {
      cls = cls + "ebk"
    }
  }
  return cls;
}

function setTABIDX(x) {
    // document.getElementById(x).style.background = "yellow";
    for (var i = 0; i < TABTABLE.length; i++) {
      item=TABTABLE[i]
      if (item.id == x) {
        TABIDX = i;
      }
    }
}

function blurFunction(x) {
    var dataval = document.getElementById(x).value
}

function keypressFunction(e, x) {
  var userkey = e.which || e.keyCode;
  var i =   document.getElementById(x);
  var o =   document.getElementById("out");
  if ((i.value.length === i.maxLength) && (userkey != 8)) {
    nextTab();
  }
  o.innerHTML = "key=" + userkey;
}

function keydownFunction(e, x) {
var userkey = e.which || e.keyCode;
  if (userkey == 9) {
    e.preventDefault();
    nextTab();
  }
  if (userkey == 40) {
    e.preventDefault();
    nextRowdown();
  }
  if (userkey == 38) {
    e.preventDefault();
    nextRowup();
  }
}

function nextRowdown() {
  currentTab = TABTABLE[TABIDX];
  newTab = TABTABLE[TABIDX];
  for (var field = TABIDX; field < TABTABLE.length; field++) {
    if (TABTABLE[field].row > currentTab.row) {
      ele = document.getElementById(TABTABLE[field].id);
      ele.focus();
      newTab = TABTABLE[TABIDX];
      break;
    }
  }
  if (newTab.row == currentTab.row) {
    TABIDX = 0;
    newTab = TABTABLE[TABIDX];
    ele = document.getElementById(newTab.id);
    ele.focus();
  }
}

function nextRowup() {
  currentTab = TABTABLE[TABIDX];
  newTab = TABTABLE[TABIDX];
  for (var field = TABIDX; field > -1; field--) {
    if (TABTABLE[field].row < currentTab.row) {
      ele = document.getElementById(TABTABLE[field].id);
      ele.focus();
      newTab = TABTABLE[TABIDX];
      break;
    }
  }
  if (newTab.row == currentTab.row) {
    TABIDX = TABTABLE.length - 1;
    newTab = TABTABLE[TABIDX];
    ele = document.getElementById(newTab.id);
    ele.focus();
  }
}

function nextTab() {
  TABIDX++;
  if (TABIDX >= TABCNT) {
    TABIDX = 0;
  }
  id = TABTABLE[TABIDX].id;
  ele = document.getElementById(id);
  ele.focus();
  // var len = ele.val().length;
  // ele.val(ele.val());
  // var status = setCaretPosition(ele, len);
}

function initform(f) {
  // Initialize form.
  clearform()
  TABTABLE = [];
  // var head = document.getElementById('descr');
  // head.innerHTML = f.descr;
  lastRow = 0;
  colnum = 0;
  for (var i = 0; i < f.dataFields.length; i++) {
    var item = f.dataFields[i];
    if (item.rownum != lastRow) {
      lastRow = item.rownum;
      colnum = 0;
    }
    colnum++;
    if (item.fieldtype != "D") {
      TABTABLE.push({
        tab: parseInt(item.scrnord),
        id: "i" + item.fieldnum,
        row: parseInt(item.rownum),
        col: colnum
      })
    }
    addInput(item);
  }

  for (var i = 0; i < f.Functionkeys.length; i++) {
      setFkey(f.Functionkeys[i])
  }
  for (var i = 0; i < f.textFields.length; i++) {
      addSpan(f.textFields[i]);
  }
  for (var i = 0; i < f.fieldSets.length; i++) {
      addFieldset(f.fieldSets[i]);
  }

  TABTABLE = TABTABLE.sort(function(a, b) {
    return parseInt(a.tab) - parseInt(b.tab);
  });
  TABIDX = 0;
  TABCNT = length = TABTABLE.length;
  id = TABTABLE[TABIDX].id;
  document.getElementById(id).focus();
}

function closemsg() {
  document.getElementById("alertbox").style.display = "none"
}
// EXAMPLE setCaretPosition(elemId, caretPos)
// $("a").click(function(e){
//     e.preventDefault();
//
//     var input = $("#test");
//
//     // since we are setting it to the END we need the .length
//     var len = input.val().length;
//
//     input.val(input.val());
//     // ^ this is used to not only get "focus", but
//     // to make sure we don't have it everything -selected-
//     // (it causes an issue in chrome, and having it doesn't hurt any other browser)
//
//     var status = setCaretPosition('test', len);
//     // Just to show whether it worked or not
//     // Obviously don't have to use the boolean return it brings back
//
//     $('#result').text(status ?  'Oh snap dawg, it worked!' : 'Fail city bitch');
// });

function setCaretPosition(el, caretPos) {
    // var el = document.getElementById(elemId);

    if (el !== null) {

        if (el.createTextRange) {
            var range = el.createTextRange();
            range.move('character', caretPos);
            range.select();
            return true;
        }

        else {
            if (el.selectionStart || el.selectionStart === 0) {
                el.focus();
                el.setSelectionRange(caretPos, caretPos);
                return true;
            }

            else  { // fail city, fortunately this never happens (as far as I've tested) :)
                el.focus();
                return false;
            }
        }
    }
}
