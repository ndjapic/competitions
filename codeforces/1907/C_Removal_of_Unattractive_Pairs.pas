program C_Removal_of_Unattractive_Pairs; {$H+}
uses
    math;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, m, ans: int32;
    ch: char;
    s: string;
    f: array ['a' .. 'z'] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);

        for ch := 'a' to 'z' do f[ch] := 0;

        for i := 1 to n do inc(f[s[i]]);

        m := 0;
        for ch := 'a' to 'z' do m := max(m, f[ch]);

        ans := max(n mod 2, m - (n-m));
        writeln(ans);

    end;
end.
