# Задатак: Anti_Palindrome.pas

```pascal
program Anti_Palindrome;
var
    notc: int32;
    n, i, c0, c1: int32;
    ans: int8;
    ch: char;
    c: array ['a' .. 'z'] of int32;

begin
    readln(notc);
    repeat

        for ch := 'a' to 'z' do c[ch] := 0;

        readln(n);
        for i := 1 to n do begin
            read(ch);
            inc(c[ch]);
        end;
        readln;

        c0 := 0;
        c1 := 0;
        for ch := 'a' to 'z' do begin
            if odd(c[ch]) then
                inc(c1)
            else if c[ch] = 0 then
                inc(c0);
        end;

        if c0 = 25 then
            ans := 1 + n mod 2
        else if c1 = n mod 2 then
            ans := 1
        else
            ans := 0;

        writeln(ans);

        dec(notc);
    until notc = 0;
end.


```
