Potential improvements to build.yml here:

- Wrap the core idea as a new GH action, https://docs.github.com/en/actions/creating-actions/about-custom-actions .
  This would allow users to use it, and then upgrade, to follow the latest best practices
  "how to build in GH actions using CGE".

- Optionally release artifacts as GH release (with some tag, or draft, to clearly communicate this is a snapshot).

  Use GH cli for release should be possible:

  https://cli.github.com/manual/gh_release_upload
    gh release upload --clobber snapshot *-win64-x86_64.zip *-linux-x86_64.tar.gz
    Beware:
    - it will not work in our Docker? Although it has GH ClI installed, but we should pass token there?
    - by default, it will not clobber files when version number or project name changes.
      Remove version number, to allow clobbering.

  See https://docs.github.com/en/actions/using-workflows/using-github-cli-in-workflows

  This should be optional, not everyone may want to put GitHub token in their secrets.
  Note: putting GitHub token in your secrets may expose them in some scenarios, see
  - https://securitylab.github.com/research/github-actions-preventing-pwn-requests/
  - https://nathandavison.com/blog/github-actions-and-the-threat-of-malicious-pull-requests
  So users should be warned about it (they shouldn't paste contents into their workflows
  without fully understanding, otherwise their secrets *can* leak in certain cases).
