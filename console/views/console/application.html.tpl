{{define "pagetitle"}}应用控制台 - {{.app.Name}}{{end}}

{{$name := .app.Name}}
<div class="panel panel-default">
  {{template "_appnav" .}}
</div>

<div class="panel panel-default">
  <div class="panel-heading">应用框架</div>
  <table class="table">
    <tr>
      <th style="width:12em;">ID</th>
      <th>名称</th>
      <th style="width:12em;">IP</th>
      <th style="width:15em;">输出端口</th>
      <th style="width:6em;">状态</th>
      <th style="width:2em;">
        <button id="refresh-btn" class="btn btn-link" type="button" style="padding:0;margin:0;" title="重新启动">
          <i class="fa fa-refresh"></i>
        </button>
      </th>
    </tr>
    {{- range .app.Frameworks}}
    <tr>
      <td>{{printf "%.12s" .ID}}</td>
      <td>{{.DisplayName}}</td>
      <td>{{.IP}}</td>
      <td>{{.Ports}}</td>
      <td><span id="{{.ID}}" class="label state state-{{.State}}">{{.State}}</span></td>
      <td></td>
    </tr>
    {{- end}}
  </table>
  <div class="panel-footer" style="padding-top:5px; padding-bottom:5px;">
    <form id="scaling-form" class="form-inline" action="/applications/{{$name}}/scale" method="POST">
      <div class="form-group">
        <p class="form-control-static">弹性伸缩&nbsp;</p>
        <input id="scaling" type="hidden" name="scale" value="{{.app.Scale}}"/>
        <div class="btn-group">
          <button id="scaleup" class="btn btn-default btn-xs" {{if ge .app.Scale 10}}disabled="disabled"{{end}} type="button">
            <i class="fa fa-plus"></i>
          </button>
          <button id="scaledown" class="btn btn-default btn-xs" {{if le .app.Scale 1}}disabled="disabled"{{end}} type="button">
            <i class="fa fa-minus"></i>
          </button>
        </div>
        <span class="badge">{{.app.Scale}}</span>
      </div>
    </form>
  </div>
</div>

<div class="panel panel-default">
  <div class="panel-heading">当前服务</div>
  {{- with .app.Services}}
  <table class="table">
    <tr>
      <th style="width:12em;">ID</th>
      <th>名称</th>
      <th style="width:12em;">IP</th>
      <th style="width:15em;">输出端口</th>
      <th style="width:6em;">状态</th>
      <th style="width:2em;"></th>
    <tr>
    {{- range .}}
    <tr>
      <td>{{printf "%.12s" .ID}}</td>
      <td>{{.DisplayName}}</td>
      <td>{{.IP}}</td>
      <td>{{.Ports}}</td>
      <td><span id="{{.ID}}" class="label state state-{{.State}}">{{.State}}</span></td>
      <td>
        <form class="form-inline" action="/applications/{{$name}}/services/{{.Name}}/delete" method="post">
          <button class="btn btn-link" type="button" style="padding:0;margin:0;" title="删除" data-toggle="modal" data-target="#confirm-modal"
                  data-message="<p>即将删除 <strong>{{.DisplayName}}</strong> 服务。</p><p>删除服务有可能使应用运行不正常，与服务相关的所有数据都将被删除，并且无法恢复，是否继续？</p>">
            <i class="fa fa-minus"></i>
          </button>
        </form>
      </td>
    </tr>
    {{- end}}
  </table>
  {{- else}}
  <div class="panel-body">无</div>
  {{- end}}
  <div class="panel-footer">
    <div class="row">
      <div class="col-md-3">
        <form action="/applications/{{$name}}/services" method="POST">
          <div class="input-group">
            {{- with .available_plugins}}
            <span class="input-group-btn">
              <button type="button" class="btn btn-default btn-sm dropdown-toggle" data-toggle="dropdown" aria-hasgroups="true" aria-expanded="false"><span class="caret"></span></button>
              <ul class="dropdown-menu">
                {{- range .}}
                <li><a name="{{.Name}}:{{.Version}}" class="plugin-select" href="#">{{.DisplayName}}</a></li>
                {{- end}}
              </ul>
            </span>
            {{- end}}
            <input type="text" id="plugins" name="plugins" class="form-control input-sm" value="{{.plugins}}" placeholder="增加服务...">
            <span class="input-group-btn">
              <button class="btn btn-primary btn-sm" type="submit"><i class="fa fa-plus"></i></button>
            </span>
          </div>
          <input type="hidden" name="csrf_token" value="{{.csrf_token}}"/>
        </form>
      </div>
    </div>
  </div>
</div>

{{- with .error}}
<div class="alert alert-danger alert-dismissible" role="alert" style="margin-top: 20px;">
  <button type="button" class="close" data-dismiss="alert" aria-label="Close">
    <span aria-hidden="true">&times;</span>
  </button>
  <p>{{.}}</p>
</div>
{{- end}}

{{- template "_danger_modal"}}
{{- template "_select_plugin"}}

<script>
$('#scaleup').on('click', function(e) {
  $('#scaling').val(Number($('#scaling').val())+1)
  $('#scaling-form').trigger('submit')
})
$('#scaledown').on('click', function(e) {
  $('#scaling').val(Number($('#scaling').val())-1)
  $('#scaling-form').trigger('submit')
})

function setStateLabelClass() {
  $('.state').addClass('label-default')
  $('.state-running').addClass('label-success')
  $('.state-building').addClass('label-info')
  $('.state-failed').addClass('label-danger')
}
setStateLabelClass()

$('#refresh-btn').on('click', function(e) {
  $.post("/applications/{{$name}}/reload/async", function(id) {
    $('#refresh-btn').attr('disabled', 'disabled')
    $('#refresh-btn i').addClass('fa-spin')
    updateStatus(id)
  })
})

function updateStatus(id) {
  var restore = function() {
    clearInterval(intervalHandle)
    $('#refresh-btn').removeAttr("disabled")
    $('#refresh-btn i').removeClass("fa-spin")
  }

  var update = function(states) {
    for (var i in states) {
      var st = states[i]
      $('#'+st.ID).text(st.State)
      $('#'+st.ID).attr("class", "label state state-"+st.State)
    }
    setStateLabelClass()
  }

  var intervalHandle = setInterval(function() {
    $.ajax({
      url: "/applications/{{$name}}/status",
      data: {id: id},
      type: "GET",
      dataType: "json",
    })
    .done(function(json) {
      update(json.States)
      if (json.Status == "finished") {
        restore()
      }
    })
    .fail(function(xhr, status, errorThrown) {
      restore()
    })
  }, 1000)
}
</script>
