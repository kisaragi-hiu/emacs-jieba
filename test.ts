import kleur from "npm:kleur";

async function test(file: string, keep?: boolean) {
  console.log("");
  console.log(kleur.green().inverse(file));
  if (!keep) {
    try {
      Deno.removeSync("jieba-dyn.so");
    } catch (_e) {
      // Ignore
    }
  }
  const process = Deno.run({
    cmd: ["cask", "emacs", "--batch", "-L", ".", "-l", file],
  });
  const status = await process.status();
  if (status.success) {
    console.log(kleur.green(`${file} ran without error`));
  } else {
    console.log(kleur.red(`${file} failed`));
  }
}

// Specify file -> run those tests
// Don't -> we're testing requring the library
if (Deno.args.length > 0) {
  for (const file of Deno.args) {
    await test(file, true);
  }
} else {
  for (const file of [
    "tests/require-download.el",
    "tests/require-compile.el",
    "tests/require-directly.el",
  ]) {
    await test(file);
  }
}

// Local Variables:
// lsp-disabled-clients: (ts-ls)
// End:
