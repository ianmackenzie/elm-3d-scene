import { test, expect } from "@playwright/test";
import path from "node:path";
import fs from "node:fs";

const unsortedCases: string[] = fs
  .readFileSync(path.join(__dirname, "portability-testing", "test_cases_2.txt"))
  .toString()
  .split("\n")
  .filter((record) => record.trim().length > 0)
  .slice(1); // remove the header

// see the logic in the testCaseArray function in TestApp.elm
const testCases = [
  ...unsortedCases.filter((name) => name.includes("Multisampling")),
  ...unsortedCases.filter((name) => !name.includes("Multisampling")),
];

for (let i = 0; i < testCases.length; i++) {
  const name = testCases[i].split(/\s+/g).join("-").toLowerCase();
  const number = i + 1;
  test(`Snapshot ${number}`, async ({ page }) => {
    await page.goto(`/TestApp.html?test_case=${number}`);
    await expect(page.getByTestId("scene")).toHaveScreenshot(
      `${number}-${name}.png`
    );
  });
}
