{{define "pagetitle"}}应用控制台 - 终端{{end}}
{{define "prelude"}}
<script type="text/javascript" src="/static/js/terminal.min.js"></script>
<style>
#term {
    background: black;
    color: white;
    font-family: Courier, monospace;
    display: inline-block;
}
</style>
{{end}}

<div class="row container">
  <pre id="term" data-columns="120" data-rows="30" contenteditable="true"></pre>
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
  var t = $('#term')[0];
  var term = new Terminal(t.dataset);
  term.state.setMode('crlf', false);
  term.state.setMode('cursor', true);
  term.state.setMode('cursorBlink', true);

  var ws = new WebSocket("{{.ws}}");
  ws.binaryType = 'arraybuffer';

  term.dom(t).on('data', function(data) {
    ws.send(data);
  });

  ws.onmessage = function(evt) {
    term.write(evt.data);
  };

  ws.onclose = function(evt) {
    $('#close-modal').on('hide.bs.modal', function(e) {
      window.location.href = "/applications/{{.name}}";
    });
    $('#close-modal').modal();
  };

  t.focus();
  $(window).on('focus', function(e) {
    t.focus();
  });
});
</script>
