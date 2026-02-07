program A_Make_it_White; {$H+}
var
    ntc, tci: int16;
    n, l, r: int8;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);

        l := 1;
        r := n;
        while (l <= r) and (s[l] = 'W') do inc(l);
        while (l <= r) and (s[r] = 'W') do dec(r);

        writeln(r-l+1);

    end;
end.
