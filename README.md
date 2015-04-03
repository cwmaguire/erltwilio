# erltwilio
Heroku Twilio Cowboy Web server to display SMS messages

This server can be hosted on Heroku as used as a target server for
Twilio SMS messages. The messages will disappear if the server dies and
as of this writing the server only holds the last 30 messages.

Use the following to setup your Heroku project to use the Erlang
buildpack:
heroku config:add BUILDPACK_URL="https://github.com/heroku/heroku-buildpack-erlang.git" -a twilio-sms-web

Example POST with Curl:
curl -X POST 'http://localhost:8080' -d 'Body=SMS_message_body&From=SMS_sending_number'
