{{define "pagetitle"}}应用控制台 - {{.app.Name}} - 设置{{end}}
{{define "prelude"}}
<script type="text/javascript" src="/static/js/terminal.min.js"></script>
<link rel="stylesheet" href="/static/css/terminal.css" />
<style>
#deploy-modal .modal-dialog {
  position: relative;
  display: table;
  overflow: auto;
  width: auto;
}
</style>
{{end}}

{{$name := .app.Name}}
<div class="panel panel-default">
  {{template "_appnav" .}}
  <div class="panel-body">
    <div class="row">
      <div class="col-md-2">域名</div>
      <div class="col-md-6">
        <div class="form-group">
          <label>公共域名</label>
          <pre>{{.app.DNS}}</pre>
        </div>
        <div>
          <label>自有域名</label>
          {{- with .app.Hosts}}
          <ul class="list-unstyled">
            {{- range .}}
            <li>
              <form class="form-inline" action="/applications/{{$name}}/host/delete" method="post">
                <div class="form-group">
                  <button class="btn btn-link" type="submit" style="padding:0;margin:0;" title="删除">
                    <i class="fa fa-minus"></i>
                  </button>
                  <input type="hidden" name="hostname" value="{{.}}"/>
                </div>
                <a href="http://{{.}}" target="_blank">{{.}}</a>
              </form>
            </li>
            {{- end}}
          </ul>
          {{- else}}
          <p class="form-control-static">如果你已经注册了域名，可以将域名添加在这里，以通过自己的域名访问应用。</p>
          {{- end}}
          <button class="btn btn-success btn-sm" data-toggle="modal" data-target="#host-input-form"><i class="fa fa-plus"></i> 增加域名</button>
        </div>
        {{- with .error}}
        <div class="col-md">
          <div class="alert alert-danger">{{.}}</div>
        </div>
        {{- end}}
      </div>
    </div>

    <hr/>
    <div class="row">
      <div class="col-md-2">应用部署</div>
      <div class="col-md-6">
       <p>可以通过<code>git</code>推送来部署应用，使用以下命令获得应用代码的副本。</p>
       <pre>$ {{.app.CloneURL}}</pre>
       <p>代码修改完成后，使用以下命令将改动推送到云端，服务器将自动完成应用的构建与部署。</p>
       <pre>$ git add .
$ git commit -am "make change"
$ git push origin master</pre>

        {{- if .app.Branches }}
        <p>此外，如有必要，也可以点击以下按钮主动触发应用部署，并且可以选择当前分支以实现快速版本切换。</p>
        <form id="deploy-form" class="form-inline" action="/applications/{{$name}}/deploy" method="post">
          <div class="form-group">
            <label for="branch">当前分支：</label>
            <div class="input-group">
              <input type="text" id="branch" name="branch" class="form-control input-sm" value="{{.app.Branch.Id}}"></input>
              <div class="input-group-btn dropdown">
                <button type="button" class="btn btn-default btn-sm dropdown-toggle" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                  <span class="caret"></span>
                </button>
                <ul class="dropdown-menu dropdown-menu-right">
                  <li class="dropdown-header">分支</li>
                  {{- range .app.Branches }}
                  {{- if eq .Type "BRANCH" }}
                  <li class="branch-select" data-input="#branch">
                    <a name="{{.Id}}" href="#">{{.DisplayId}}</a>
                  </li>
                  {{- end }}
                  {{- end }}
                  <li class="dropdown-header">标签</li>
                  {{- range .app.Branches }}
                  {{- if eq .Type "TAG" }}
                  <li class="branch-select" data-input="#branch">
                    <a name="{{.Id}}" href="#">{{.DisplayId}}</a>
                  </li>
                  {{- end }}
                  {{- end }}
                </ul>
              </div>
            </div>
            <button id="deploy-btn" class="btn btn-success btn-sm" type="submit">
              <i class="fa fa-cloud-upload"></i> 立即部署
            </button>
          </div>
        </form>
        {{- else }}
        <p>此外，如有必要，也可以点击以下按钮主动触发应用部署。</p>
        <form id="deploy-form" class="form-inline" action="/applications/{{$name}}/deploy" method="post">
          <button id="deploy-btn" class="btn btn-success btn-sm" type="submit">
            <i class="fa fa-cloud-upload"></i> 立即部署
          </button>
        </form>
        {{- end }}
      </div>
    </div>

    <hr/>
    <div class="row">
      <div class="col-md-2">应用管理</div>
      <div class="col-md-6">
        <p>使用云途命令行工具可以完全控制应用运行环境。请从以下地址下载命令行工具：</p>
        <ul>
          <li><a href='{{download "linux"}}'>Linux</a></li>
          <li><a href='{{download "darwin"}}'>Mac OS X</a></li>
          <li><a href='{{download "freebsd"}}'>FreeBSD</a></li>
          <li><a href='{{download "windows"}}'>Windows</a></li>
        </ul>
        <p>下载后运行<code>cwcli help</code>可查看命令帮助，运行<code>cwcli help app</code>可查看应用管理命令的更多帮助。</p>
      </div>
    </div>

    <hr/>
    <div class="row">
      <div class="col-md-2">删除应用</div>
      <div class="col-md-6">
        <p>此操作无法恢复，请确定已做好备份</p>
        <form action="/applications/{{$name}}/delete" method="post">
          <button class="btn btn-danger" type="button" data-toggle="modal"
                  data-target="#confirm-modal" data-message="与应用相关的所有数据都将被删除，并且无法恢复，是否继续？">
            <i class="fa fa-trash-o fa-lg"></i> 删除应用...
          </button>
        </form>
      </div>
    </div>
  </div>
</div>

<div class="modal" id="host-input-form" tabindex="-1" role="dialog">
  <div class="modal-dialog" role="document">
    <div class="modal-content">
      <form action="/applications/{{$name}}/host" method="post">
        <div class="modal-header">
          <button type="button" class="close" data-dismiss="modal"><span>&times;</span></button>
          <h4 class="modal-title">增加域名</h4>
        </div>
        <div class="modal-body">
          <div class="form-group">
            <label for="hostname" class="control-label">域名</label>
            <input type="text" class="form-control" id="hostname" name="hostname"/>
          </div>
        </div>
        <div class="modal-footer">
          <button type="submit" class="btn btn-primary">确认</button>
          <button type="button" class="btn btn-default" data-dismiss="modal">取消</button>
        </div>
      </form>
    </div>
  </div>
</div>

<div class="modal" id="deploy-modal" role="dialog">
  <div class="modal-dialog" role="document">
    <div class="modal-content">
      <div class="modal-header">
        <h4>应用部署</h4>
      </div>
      <div class="modal-body">
        <pre id="deploy-term" class="terminal" data-columns="80" data-rows="24"></pre>
      </div>
      <div class="modal-footer">
        <button id="deploy-close-btn" type="button" class="btn btn-default" data-dismiss="modal">关闭</button>
      </div>
    </div>
  </div>
</div>

{{template "_danger_modal"}}

<script>
$('#deploy-form').submit(function(e) {
  $('#deploy-modal').modal();
  e.preventDefault();
});

$('#deploy-modal').on('show.bs.modal', function(e) {
  $('#deploy-close-btn').prop('disabled', true);

  var t = $('#deploy-term')[0];
  t.innerHTML = '';

  var term = new Terminal(t.dataset);
  term.state.setMode('crlf', true);
  term.state.setMode('cursor', false);
  term.dom(t);
  term.write('');

  var wsurl = "{{.app.WS}}/applications/{{$name}}/deploy?" + $('#deploy-form').serialize();
  var ws = new WebSocket(wsurl);
  var err;

  ws.onmessage = function(evt) {
    var data = JSON.parse(evt.data);
    if (data.msg) {
      term.write(data.msg);
    }
    if (data.err) {
      term.write("\x1b[31;1m" + data.err + "\x1b[0m\n");
      err = true;
    }
  };

  ws.onclose = function(evt) {
    if (!err) {
      term.write("\n\x1b[32;1m应用部署成功\x1b[0m\n");
    }
    $('#deploy-close-btn').prop('disabled', false);
  };
});
</script>

<script>
$('.branch-select a').on('click', function(e) {
  var select = $(this).parent();
  var input = $(select.data('input'));
  input.val(this.name);
});
</script>
