{{define "pagetitle"}}应用控制台 - 设置{{end}}

<div class="row container">
{{if .user.Namespace}}
  <div class="panel panel-info col-md-12" style="margin-bottom: 20px;">
    <h4>名字空间</h4>
    <p>通过名字空间组织你的应用，并赋予每个应用一个唯一的域名</p>
    <div class="col-md">
      <form class="form-inline" action="/settings/namespace/delete" method="POST">
        <p class="form-control-static">当前正在使用的名字空间：{{.user.Namespace}}</p>
        {{if eq (len .user.Applications) 0}}
        <button class="btn btn-link" type="submit" title="删除">
          <i class="fa fa-remove"></i>
        </button>
        {{end}}
      </form>
    </div>
  </div>

  <div class="panel panel-info col-md-12">
    <h4>SSH公共密钥</h4>
    <p>通过SSH公共密钥安全上传代码</p>

    <div class="col-md-12" style="margin-top: 10px;">
      {{if ne (len .sshkeys) 0}}
      <div class="panel panel-default">
        <table class="table">
          <tr><th>标签</th><th>内容</th><th></th></tr>
          {{range .sshkeys}}
          <tr>
            <td>{{.Label}}</td>
            <td>{{printf "%.64s..." .Text}}</td>
            <td>
              <form action="/settings/sshkey/delete?key={{.Text}}" method="POST">
                <button class="btn btn-link" type="submit" title="删除" style="padding:0;margin:0;">
                  <i class="fa fa-minus"></i>
                </button>
              </form>
            </td>
          </tr>
          {{end}}
        </table>
      </div>
      {{end}}

      <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-2">
          <a class="btn btn-primary" href="/settings/sshkey"><i class="fa fa-plus"></i> 增加新密钥...</a>
        </div>
      </div>
    </div>
  </div>

{{else}}

  <div class="panel panel-info col-md-12">
    <h4>名字空间</h4>
    <p>通过名字空间组织你的应用，并赋予每个应用一个唯一的域名</p>

    <div class="row">
      <div class="col-md-4" style="margin-bottom:20px;">
        <form action="/settings/namespace" method="post">
          <div class="input-group">
            <input name="namespace" type="text" class="form-control" size="20" />
            <span class="input-group-btn">
              <button class="btn btn-primary" type="submit">创建名字空间...</button>
            </span>
            <input type="hidden" name="csrf_token" value="{{.csrf_token}}" />
          </div>
        </form>
      </div>
    </div>
    {{if .error}}
    <div class="alert alert-danger">{{.error}}</div>
    {{end}}
  </div>
{{end}}
</div>
