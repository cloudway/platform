{{define "pagetitle"}}应用控制台 - 终端{{end}}
{{define "prelude"}}
<script type="text/javascript" src="/static/js/xterm.js"></script>
<script type="text/javascript" src="/static/js/xterm/fit.js"></script>
<link rel="stylesheet" href="/static/css/xterm.css" />
<link rel="stylesheet" href="/static/css/shell.css" />
{{end}}

<div id="term-div" class="row container normal">
  <div class="term-title">
    <span class="term-title-item">{{printf "%.12s" .id}}</span><span class="term-title-item">{{.name}}</span>
    {{- if .service}}<span class="term-title-item">{{.service}}</span>{{end}}<span id="term-title" style="padding-left:10px;"></span>
    <a href="#" id="btn-fullscreen" title="全屏幕"><i class="fa fa-toggle-up"></i></a>
  </div>
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

  term.on('title', function(title) {
    $('#term-title').text(title);
  });

  var socket = new WebSocket("{{.ws}}");
  socket.binaryType = 'arraybuffer';

  var geometry = term.proposeGeometry();
  var cols = geometry.cols
  var rows = geometry.rows;
  var execId = "";

  var resizeTerm = function() {
    var geometry = term.proposeGeometry();
    if ($('#term-div').hasClass('fullscreen')) {
      geometry.rows--; // workaround
      term.resize(geometry.cols, geometry.rows);
    }
    if (geometry.cols != cols || geometry.rows != rows) {
      cols = geometry.cols;
      rows = geometry.rows;
      $.post("/shell/"+execId+"/resize", {cols: cols, rows: rows});
    }
  };

  // receive termainl ID and send terminal size
  socket.onopen = function(evt) {
    socket.send(JSON.stringify({Width: cols, Height: rows}));

    term.on('data', function(data) {
      socket.send(data);
    });

    $(window).resize(resizeTerm);

    $('#btn-fullscreen').on('click', function() {
      $('#term-div').toggleClass('fullscreen');
      $('#term-div').toggleClass('normal');
      $('#btn-fullscreen i').toggleClass('fa-toggle-up');
      $('#btn-fullscreen i').toggleClass('fa-toggle-down');

      term.fit();
      resizeTerm();
      term.focus();
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
