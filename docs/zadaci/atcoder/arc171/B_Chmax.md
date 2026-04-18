# Задатак: B_Chmax.pas

```pascal
program B_Chmax;
var
    ntc, tci, n, a, b, h0, h1, d: int32;
    ans: boolean;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, a, b);
        h0 := n div 2;
        h1 := n - h0;
        d := n-a;
        ans := d >= 0;

        if not ans then
        else begin
            if a >= h0 then begin
                dec(a, h0);
                dec(h1, a);
            end;
            ans := b <= d*h1;
        end;

        if ans then
            writeln('Yes')
        else
            writeln('No');

    end;
end.

```
