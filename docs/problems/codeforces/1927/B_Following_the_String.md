# Problem: B_Following_the_String.pas

```pascal
program B_Following_the_String; {$H+}
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, i: int32;
    s: string;
    ch: char;
    a: array [1 .. maxn] of int32;
    c: array ['a' .. 'z'] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        setlength(s, n);
        for ch := 'a' to 'z' do c[ch] := 0;

        for i := 1 to n do begin
            read(a[i]);
            ch := 'a';
            while (ch < 'z') and (c[ch] <> a[i]) do ch := succ(ch);
            s[i] := ch;
            inc(c[ch]);
        end;
        readln;

        writeln(s);

    end;
end.

```
