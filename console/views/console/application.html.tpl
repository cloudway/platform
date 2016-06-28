{{define "pagetitle"}}应用控制台 - {{.app.Name}}{{end}}

{{$name := .app.Name}}
<div class="panel panel-default">
  <div class="panel-body">
    <div class="row container">
      <div class="col-md-8">
        <h3><a href="{{.app.URL}}" target="_blank">{{.app.URL}}</a></h3>
      </div>
      <div class="col-md-4 text-right">
        <form action="/applications/{{$name}}/reload" method="post">
          <button class="btn btn-link" type="submit" title="重新启动"><i class="fa fa-refresh"></i></button>
        </form>
      </div>
    </div>
  </div>
</div>

<div class="panel panel-default">
  <div class="panel-heading">当前服务</div>
  <table class="table">
    <tr>
      <th style="width:12em;">ID</th>
      <th>名称</th>
      <th style="width:12em;">IP</th>
      <th style="width:12em;">输出端口</th>
      <th style="width:10em;">状态</th>
      <th style="width:1em;"></th>
    </tr>
    {{range .app.Services}}
    <tr>
      <td>{{.ID}}</td>
      <td>{{.DisplayName}}</td>
      <td>{{.IP}}</td>
      <td>{{.Ports}}</td>
      <td>{{.State}}</td>
      <td>
        {{if ne .Category "Framework"}}
        <form class="form-inline" action="/applications/{{$name}}/services/{{.Name}}/delete" method="post">
          <button class="btn btn-link" type="button" style="padding:0;margin:0;" title="删除" data-toggle="modal"
                  data-target="#confirm-modal" data-message="删除服务有可能使应用运行不正常，与服务相关的所有数据都将被删除，并且无法恢复，是否继续？">
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
        <input type="text" name="plugins" class="form-control" value="{{.plugins}}" placeholder="增加服务...">
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
