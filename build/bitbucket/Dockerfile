FROM bitbucket-server:deps

 # install cloudway repository deployer
COPY cwman /usr/bin/cwman
COPY repo-deployer-*.jar repo-deployer.jar
COPY entrypoint.sh install-plugin.sh ./

EXPOSE 7990 7999
ENTRYPOINT ["./entrypoint.sh"]
CMD ["bitbucket-server"]
