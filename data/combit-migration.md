# Combit Export

- Organisation -> Export
- Vorlage "for-ghm" oder manuell:
  ANSII | CR+LF | Escape "" | Sep , | Dez . | Dat 2018-10-28

# Conversion

```
$ file combit-for-ghm.20181028.csv
combit-for-ghm.20181028.csv: ISO-8859 text, with very long lines, with CRLF line terminators
$ iconv -f ISO-8859-1 -t utf-8 > combit.csv
$ dos2unix combit.csv
$ file combit.csv
combit.csv: UTF-8 Unicode text, with very long lines
```

# Import

```
make import
```
