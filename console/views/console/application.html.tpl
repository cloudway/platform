{{define "pagetitle"}}应用控制台 - {{.app.Name}}{{end}}

{{$name := .app.Name}}
<div class="panel panel-default">
  <div class="panel-body">
    <h3><a href="{{.app.URL}}" target="_blank">{{.app.URL}}</a></h3>
  </div>
</div>

<div class="panel panel-default">
  <div class="panel-heading">当前服务</div>
  <table class="table table-hover">
    <tr>
      <th style="width:12em;">ID</th>
      <th>名称</th>
      <th style="width:12em;">IP</th>
      <th style="width:15em;">输出端口</th>
      <th style="width:6em;">状态</th>
      <th style="width:2em;">
        <form class="form-inline" action="/applications/{{$name}}/reload" method="post">
          <button class="btn btn-link" type="submit" style="padding:0;margin:0;" title="重新启动">
            <i class="fa fa-refresh"></i>
          </button>
        </form>
      </th>
    </tr>
    {{range .app.Services}}
    <tr>
      <td>{{printf "%.12s" .ID}}</td>
      <td>{{.DisplayName}}</td>
      <td>{{.IP}}</td>
      <td>{{.Ports}}</td>
      <td><span class="label state state-{{.State}}">{{.State}}</span></td>
      <td>
        {{if ne .Category "Framework"}}
        <form class="form-inline" action="/applications/{{$name}}/services/{{.Name}}/delete" method="post">
          <button class="btn btn-link" type="button" style="padding:0;margin:0;" title="删除" data-toggle="modal" data-target="#confirm-modal"
                  data-message="<p>即将删除 <strong>{{.DisplayName}}</strong> 服务。</p><p>删除服务有可能使应用运行不正常，与服务相关的所有数据都将被删除，并且无法恢复，是否继续？</p>">
            <i class="fa fa-minus"></i>
          </button>
        </form>
        {{end}}
      </td>
    </tr>
    {{end}}
  </table>
</div>

<div class="row">
  <div class="col-md-3">
    <form action="/applications/{{$name}}/services" method="POST">
      <div class="input-group">
        {{- with .available_plugins}}
        <span class="input-group-btn">
          <button type="button" class="btn btn-default dropdown-toggle" data-toggle="dropdown" aria-hasgroups="true" aria-expanded="false"><span class="caret"></span></button>
          <ul class="dropdown-menu">
            {{- range .}}
            <li><a name="{{.Name}}:{{.Version}}" class="plugin-select" href="#">{{.DisplayName}}</a></li>
            {{- end}}
          </ul>
        </span>
        {{- end}}
        <input type="text" id="plugins" name="plugins" class="form-control" value="{{.plugins}}" placeholder="增加服务...">
        <span class="input-group-btn">
          <button class="btn btn-primary" type="submit"><i class="fa fa-plus"></i></button>
        </span>
      </div>
      <input type="hidden" name="csrf_token" value="{{.csrf_token}}"/>
    </form>
  </div>
  <div class="col-md-9 text-right">
    <form action="/applications/{{$name}}/delete" method="POST">
      <button class="btn btn-danger" type="button" data-toggle="modal"
              data-target="#confirm-modal" data-message="与应用相关的所有数据都将被删除，并且无法恢复，是否继续？">
        <i class="fa fa-trash-o fa-lg"></i> 删除应用...
      </button>
    </form>
  </div>
</div>

{{with .error}}
<div class="alert alert-danger alert-dismissible" role="alert" style="margin-top: 20px;">
  <button type="button" class="close" data-dismiss="alert" aria-label="Close">
    <span aria-hidden="true">&times;</span>
  </button>
  <p>{{.}}</p>
</div>
{{end}}

{{template "_modal"}}
{{template "_select_plugin"}}
<script>
$('.state').addClass('label-default')
$('.state-running').addClass('label-success')
$('.state-building').addClass('label-info')
$('.state-failed').addClass('label-danger')
</script>
