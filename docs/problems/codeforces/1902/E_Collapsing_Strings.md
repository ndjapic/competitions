# Problem: E_Collapsing_Strings.pas

```pascal
program E_Collapsing_Strings;
const
    maxn = 1000 * 1000;
var
    n, i: int32;
    ch: char;
    x, ans: int32;
    lc, rc: array ['a' .. 'z'] of int32;

begin
    for ch := 'a' to 'z' do begin
        lc[ch] := 0;
        rc[ch] := 0;
    end;

    readln(n);
    x := n;

    for i := 1 to n do begin
        read(ch);
        inc(lc[ch]);

        while not eoln do begin
            read(ch);
            inc(x);
        end;
        readln;

        inc(rc[ch]);
    end;

    ans := 2*n*x;
    for ch := 'a' to 'z' do dec(ans, rc[ch] * lc[ch]);
    writeln(ans);
end.

```
