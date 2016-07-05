{{define "pagetitle"}}应用控制台 - {{.app.Name}} - 设置{{end}}

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
       <p>在完成对代码的修改后，使用以下命令将改动推送到云端，服务器将自动完成应用的构建与部署。</p>
       <pre>$ git add .
$ git commit -am "make change"
$ git push origin master</pre>
        <p>此外，如有必要，也可以点击以下按钮主动触发应用部署。</p>
        <button id="deploy-btn" class="btn btn-success btn-sm" type="button">
          <i class="fa fa-cloud-upload"></i> 立即部署
        </button>
        <span id="deploy-spinner" class="hidden"><i class="fa fa-spinner fa-spin"></i></span>
      </div>
    </div>

    <hr/>
    <div class="row">
      <div class="col-md-2">删除应用</div>
      <div class="col-md-6">
        <p>此操作无法恢复，请确定已做好备份</p>
        <form action="/applications/{{$name}}/delete" method="POST">
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
            <label for="host-name" class="control-label">域名</label>
            <input type="text" class="form-control" name="hostname"/>
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

<div class="modal" id="deploy-confirm-modal" role="dialog">
  <div class="modal-dialog" role="document">
    <div class="modal-content">
      <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal"><span>&times;</span></button>
        <h4 class="modal-title">继续</h4>
      </div>
      <div class="modal-body">
        <p>已触发应用构建，构建过程将在后台进行，构建成功并完成部署后才能继续访问你的应用。</p>
      </div>
      <div class="modal-footer">
        <button type="button" class="btn btn-default" data-dismiss="modal">关闭</button>
      </div>
    </div>
  </div>
</div>

{{template "_danger_modal"}}

<script>
$('#deploy-btn').on('click', function(e) {
  $('#deploy-spinner').removeClass("hidden")
  $.post("/applications/{{$name}}/deploy", null, function(resp) {
    $('#deploy-spinner').addClass("hidden")
    $('#deploy-confirm-modal').modal({})
  })
})
</script>
