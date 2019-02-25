payloadserver.go contains the ``Payload`` function that gets deployed. To deploy
it from the command line, run:

		$ gcloud functions deploy payloadserver --entry-point Payload --runtime go111 --trigger-http --set-env-vars HOOK_SECRET_KEY=<key>

Then point the Github webhook to it, and it should work.

To check logs from the command-line, run:

		$ gcloud functions logs read payloadserver

