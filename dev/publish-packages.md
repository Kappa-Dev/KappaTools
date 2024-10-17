## Release process

There are different releases to make:
- opam
- github
- pip

Additionnally, nightlies are automatically released from _master_ branch on https://kappalanguage.org/

### Opam
#### Test
```bash
opam install .
```
...

#### Setup version in git
set env var
```bash
VER=v4.1.3 && echo $VER
```

```bash
git tag -a $VER -m "Kappa Software Suite version $VER" && git push origin $VER
```
Can use simple tag text:


if need to undo as some changes are necessary before full publish?

```bash
git tag -d $VER && git push --delete origin $VER
```

#### Opam publish

We publish all except kappa-webapp at the moment. (Let's add the webapp to opam when we are able to build it there.)

```bash
opam publish kappa-agents.opam kappa-binaries.opam kappa-library.opam
```

enter token, if no valid token availaible, create with only public-repo permission 

then validate commit, validate the PR creation, check the CI

if it fails fix things, push to master, remove and reset the tag and start again


TODO: webapp


### Github

Create a new release draft

- Title:
```
Kappa Software Suite version v4.1.3
```

- Text:
```
v4.1.3

Kappa Software Suite version v4.1.3
```

Set last release as previous tag, then click _Generate release notes_ and edit results to keep what's relevant

Add files for:

```
Kappapp_for_linux.tar.gz
Kappapp_for_mac_os_10.15.zip
Kappapp_for_windows.zip
```
taken from nightly builds: https://tools.kappalanguage.org/nightly-builds/

Source code from release tag https://github.com/Kappa-Dev/KappaTools/releases/tag/v4.1.3 should appear automatically

```
Source code (zip)
Source code (tar.gz) 
```



### Pip

TODO
