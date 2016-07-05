<div class="row">
  <div class="col-md-offset-1 col-md-10">
    {{if .error}}
    <div class="alert alert-danger">{{.error}}</div>
    {{end}}
    <form method="post">
      <div class="form-group">
        <textarea class="form-control" name="content" cols="100" rows="5"></textarea>
      </div>
      <input type="hidden" name="csrf_token" value="{{.csrf_token}}"/>
      <div class="row text-right">
        <button class="btn btn-success" type="submit">保存</button>
        <a class="btn btn-link" href="/settings">取消</a>
      </div>
    </form>
  </div>
</div>
