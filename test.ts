import kleur from "npm:kleur";

async function test(file: string) {
  console.log("");
  console.log(kleur.green().inverse(file));
  try {
    Deno.removeSync("jieba-dyn.so");
  } catch (_e) {
    // Ignore
  }
  const process = Deno.run({
    cmd: ["cask", "emacs", "--batch", "-l", file],
  });
  const status = await process.status();
  if (status.success) {
    console.log(kleur.green(`${file} ran without error`));
  }
}

for (const file of [
  "tests/require-download.el",
  "tests/require-compile.el",
  "tests/require-directly.el",
]) {
  await test(file);
}

// Local Variables:
// lsp-disabled-clients: (ts-ls)
// End:
