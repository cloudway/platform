{{define "pagetitle"}}应用控制台 - 终端{{end}}
{{define "prelude"}}
<script type="text/javascript" src="/static/js/xterm.js"></script>
<script type="text/javascript" src="/static/js/xterm/fit.js"></script>
<link rel="stylesheet" href="/static/css/xterm.css" />
<style>
html, body {
  height: 100%;
}
#term-div {
  height: 80%;
  min-height: 80%;
}
#term-container {
  padding: 8px;
  background: black;
  height: 100%;
  min-height: 100%;
}
#term {
  height: 100%;
  min-height: 100%;
}
</style>
{{end}}

<div id="term-div" class="row container">
  <div id="term-container">
    <div id="term"></div>
  </div>
</div>

<div id="close-modal" class="modal" tabindex="-1" role="dialog">
  <div class="modal-dialog" role="document">
    <div class="modal-content">
      <div class="modal-header">
        <h4 class="modal-title">关闭</h4>
      </div>
      <div class="modal-body">
        <p>远程会话已结束</p>
      </div>
      <div class="modal-footer">
        <button type="button" class="btn btn-default" data-dismiss="modal">关闭</button>
      </div>
    </div>
  </div>
</div>

<script>
$(document).on('ready', function() {
  var container = document.getElementById('term');
  var term = new Terminal({cursorBlink: true});
  term.open(container);
  term.fit();

  var socket = new WebSocket("{{.ws}}");
  socket.binaryType = 'arraybuffer';

  var geometry = term.proposeGeometry();
  var cols = geometry.cols
  var rows = geometry.rows;
  var execId = "";

  // receive termainl ID and send terminal size
  socket.onopen = function(evt) {
    socket.send(JSON.stringify({Width: cols, Height: rows}));

    term.on('data', function(data) {
      socket.send(data);
    });

    $(window).resize(function() {
      var geometry = term.proposeGeometry();
      if (geometry.cols != cols || geometry.rows != rows) {
        cols = geometry.cols;
        rows = geometry.rows;
        $.post("/shell/"+execId+"/resize", {cols: cols, rows: rows});
      }
    });
  };

  socket.onmessage = function(evt) {
    if (execId === "") {
      execId = JSON.parse(evt.data).Id;
    } else {
      term.write(evt.data);
    }
  };

  socket.onclose = function(evt) {
    $('#close-modal').modal({keyboard: true});
  };

  window.onbeforeunload = function() {
    return "您确定要离开此页面吗?"
  };

  $('#close-modal').on('hide.bs.modal', function(e) {
    window.onbeforeunload = null;
    window.location.href = "/applications/{{.name}}";
  });
});
</script>
