var fields = ['sender','recipient','subject','sidebar','body','attachments'];

function fieldId (fld) {
  return "editable-" + fld;
}

function getState () {
  var state = { };
  fields.forEach(function (el) {
    state[el] = document.getElementById(fieldId(el)).innerHTML;
  });
  return state;
}

function setState (state) {
  fields.forEach(function (el) {
    document.getElementById(fieldId(el)).innerHTML = state[el];
  });
}

// b64 encoding: https://stackoverflow.com/a/30106551

function b64EncodeUnicode(str) {
    // first we use encodeURIComponent to get percent-encoded UTF-8,
    // then we convert the percent encodings into raw bytes which
    // can be fed into btoa.
    return btoa(encodeURIComponent(str).replace(/%([0-9A-F]{2})/g,
        function toSolidBytes(match, p1) {
            return String.fromCharCode('0x' + p1);
    }));
}

function b64DecodeUnicode(str) {
    // Going backwards: from bytestream, to percent-encoding, to original string.
    return decodeURIComponent(atob(str).split('').map(function(c) {
        return '%' + ('00' + c.charCodeAt(0).toString(16)).slice(-2);
    }).join(''));
}

// safe content to hash for bookmarking
var lastHash = ""

function safe () {
  var state = getState();
  document.title = "Brief: " + state.subject
  nextHash = b64EncodeUnicode(JSON.stringify(state));
  if (lastHash != nextHash) {
    document.location.hash = nextHash;
    lastHash = nextHash;
  }
}

// load content from hash
function load () {
  var b64 = document.location.hash.substr(1);
  if (b64.length > 0 && lastHash != b64) {
    var state = JSON.parse(b64DecodeUnicode(b64));
    setState(state);
    lastHash = b64;
  }
}

function main () {
  // trigger safe after 1 seconds of no input
  var timer
  document.body.addEventListener("input", function() {
    window.clearTimeout(timer);
    timer = setTimeout(safe, 1000);

  }, false);

  // respect forward/backward navigation
  window.onhashchange = load;

  load ();
  safe ();
}

function setBackground () {
  var cb = document.getElementById("bg-checkbox");
  var bg = document.getElementById("background");
  function update () {
    var visibility = 'hidden';
    if (cb.checked) {
      visibility = 'visible';
    }
    bg.style.visibility=visibility
  }
  cb.onchange = update;
  update ();
}

function setMarks () {
  var cb = document.getElementById("marks-checkbox");
  var marks = document.querySelectorAll('.mark');
  function update () {
    var visibility = 'hidden';
    if (cb.checked) {
      visibility = 'visible';
    }
    marks.forEach(function (element) {
      element.style.visibility=visibility;
    })
  }
  cb.onchange = update;
  update ();
}

main ();
setBackground ();
setMarks ();
