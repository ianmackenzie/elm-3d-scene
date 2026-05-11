# Snapshot testing

```bash
# install the dependencies
npm ci

# install playwright browsers
npx playwright install --with-deps

# precompile the test app to `TestApp.html`
npx elm make TestApp.elm --output=TestApp.html

# run the snapshot tests
npx playwright test

# updates snapshots with chromium
npx playwright test --project=chromium --update-snapshots
```

- The tests are using [Playwright](https://playwright.dev)
- The compiled "TestApp.html" is served with [http-server](https://www.npmjs.com/package/http-server), see the `webServer` section in [playwright.config.ts](./playwright.config.ts).
