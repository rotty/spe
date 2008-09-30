BEGIN {
    IMPL_LIST = "ikarus mzscheme larceny";
    
    IMPL_RX = IMPL_LIST;
    gsub(" ", "|", IMPL_RX)
    IMPL_RX = "(" IMPL_RX  ")";
    IMPLEMENTATION = ARGV[1];
    delete ARGV[1];
    print "("; 
}

/\.sls/ && ($0 !~ /\/_darcs\//) && ($0 !~ ("\\." IMPL_RX "\\.sls$")) {
    split($0, parts, "/");
    impl_found = 0;
    impl_filename = $0;
    sub("\\.sls$", "." IMPLEMENTATION ".sls", impl_filename);
    if (system("test -f '" impl_filename "'") != 0) {
        impl_filename = $0;
    }
    sub(parts[1] "/" parts[2] "/", "", impl_filename);
    printf("(\"%s/%s\" \"%s\")\n",  parts[1], parts[2], impl_filename);
}

END {
    print ")";
}
