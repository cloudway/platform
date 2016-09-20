{{define "pagetitle"}}应用控制台 - 创建应用{{end}}
{{define "prelude"}}
<script type="text/javascript" src="/static/js/xterm.js"></script>
<script type="text/javascript" src="/static/js/xterm/fit.js"></script>
<link rel="stylesheet" href="/static/css/xterm.css" />
<style>
html, body {
  height: 100%;
}
#term-div {
  height: 70%;
  min-height: 70%;
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

<div class="row">
  <div class="col-md-6">
    <h3>创建应用</h3>
  </div>
</div>

<div id="form-div" class="row container">
  <div class="panel panel-info col-md-offset-1 col-md-6">
    <div class="panel-body">
      <form id="create-form" action="/applications/create" method="post">
        <div class="form-group">
          <label for="name">应用名称：</label>
          <div class="input-group col-md-12">
            <span class="input-group-addon">http://</span>
            <input type="text" id="name" name="name" class="form-control"/>
            <span class="input-group-addon">-{{.user.Namespace}}.{{.domain}}</span>
          </div>
        </div>
        <div class="form-group">
          <label for="framework">应用框架：</label>
          <div class="input-group col-md-12">
            <input type="text" id="framework" name="framework" class="form-control"/>
            {{- with .plugins}}
            <div class="input-group-btn">
              <button type="button" class="btn btn-default dropdown-toggle" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                <span class="caret"></span>
              </button>
              <ul class="dropdown-menu">
                {{- range .}}
                {{- if eq .Category "Framework"}}
                <li class="plugin-select" data-input="#framework" data-single-select="true">
                  <a name="{{.Name}}:{{.Version}}" href="#"><img src="{{logo .Tag .Logo}}"/> {{.DisplayName}}</a>
                </li>
                {{- end}}
                {{- end}}
              </ul>
            </div>
            {{- end}}
          </div>
        </div>
        <div class="form-group">
          <label for="services">服务：</label>
          <div class="input-group col-md-12">
            <input type="text" id="services" name="services" class="form-control"/>
            {{- with .plugins}}
            <div class="input-group-btn">
              <button type="button" class="btn btn-default dropdown-toggle" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                <span class="caret"></span>
              </button>
              <ul class="dropdown-menu">
                {{- range .}}
                {{- if eq .Category "Service"}}
                <li class="plugin-select" data-input="#services">
                  <a name="{{.Name}}:{{.Version}}" href="#"><img src="{{logo .Tag .Logo}}"/> {{.DisplayName}}</a>
                </li>
                {{- end}}
                {{- end}}
              </ul>
            </div>
            {{- end}}
          </div>
        </div>
        <div class="form-group">
          <label for="repo">代码库：</label>
          <div class="input-group col-md-12">
            <input type="text" id="repo" name="repo" class="form-control" placeholder="git://github.com/cloudway/" value="{{.repo}}"/>
          </div>
        </div>
        <input type="hidden" name="csrf_token" value="{{.csrf_token}}"/>
        <button class="btn btn-success" type="submit">创建</button>
        <a class="btn btn-link" href="/applications">取消</a>
      </form>
    </div>
  </div>
</div>

<div id="term-div" class="row container hidden">
  <div id="term-container">
    <div id="term"></div>
  </div>
  <div class="col-md-12" style="margin-top:20px;">
    <button id="return-btn" type="button" class="btn btn-primary hidden">返回</button>
  </div>
</div>

{{template "_select_plugin"}}

<script>
  $('#create-form').submit(function(e) {
    e.preventDefault();

    var wsurl = '{{.ws}}?' + $('#create-form').serialize();
    var ws = new WebSocket(wsurl);
    var term, err

    ws.onopen = function(evt) {
      $('#return-btn').addClass('hidden');
      $('#form-div').addClass('hidden');
      $('#term-div').removeClass('hidden');

      var container = document.getElementById('term');
      container.innerHTML = '';
      term = new Terminal({convertEol:true});
      term.cursorHidden = true;
      term.open(container);
      term.fit();
    };

    ws.onmessage = function(evt) {
      var data = JSON.parse(evt.data)
      if (data.msg) {
        term.write(data.msg);
      }
      if (data.err) {
        term.write("\x1b[31;1m" + data.err + "\x1b[0m\n");
        err = true;
      }
    };

    ws.onclose = function(evt) {
      $('#return-btn').removeClass('hidden');
      if (err) {
        $('#return-btn').on('click', function(e) {
          $('#form-div').removeClass('hidden');
          $('#term-div').addClass('hidden');
        });
      } else {
        term.write("\n\x1b[32;1m应用创建成功\x1b[0m\n");
        $('#return-btn').on('click', function(e) {
          window.location.href = '/applications/' + $('#name').val();
        });
      }
    };
  });
</script>
