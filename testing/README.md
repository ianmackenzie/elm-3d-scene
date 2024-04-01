# Snapshot testing

```bash
npm ci  # installs the dependencies
npx playwright install --with-deps  # installs automated browsers
npm run precompile-elm-app  # precompiles the test app to `TestApp.html`
npm test  # runs the snapshot tests
npm run update-snapshots  # updates snapshots with chromium
```

- The tests are using [Playwright](https://playwright.dev)
- The compiled "TestApp.html" is served with [http-server](https://www.npmjs.com/package/http-server), see the `webServer` section in [playwright.config.ts](./playwright.config.ts).
