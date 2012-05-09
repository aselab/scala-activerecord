set SCRIPT_DIR=%~dp0
set JAVA_TOOL_OPTIONS='-Dfile.encoding=UTF8'
java -XX:+UseConcMarkSweepGC -XX:+CMSPermGenSweepingEnabled -XX:+CMSClassUnloadingEnabled -Xss2M -XX:MaxPermSize=256m -Xmx512M -jar "%SCRIPT_DIR%sbt-launch.jar" %*
