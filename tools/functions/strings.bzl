"""Public definitions for OBazl tools functionx.

All public rules and tools imported and re-exported in this file.

Definitions outside this file are private unless otherwise noted, and
may change without notice.
"""

#############################
def _tokenize(s, sep=""):
    tokens = []
    if sep == "":
        s = s.replace("\n", " ").replace("\t", " ")
        sep = " "
    for token in s.split(sep):
        if token != "":
            tokens.append(token)
    return tokens

tokenize = _tokenize
