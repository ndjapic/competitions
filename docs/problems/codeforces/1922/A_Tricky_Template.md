# Problem: A_Tricky_Template.pas

```pascal
program A_Tricky_Template;
{$H+}
const
    maxn = 20;
var
	ntc, tci: int16;
    n, i: int8;
    found: boolean;
    a, b, c: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);
		readln(a);
		readln(b);
		readln(c);

        found := false;
        for i := 1 to n do
            if found then
            else if a[i] = b[i] then
                found := c[i] <> a[i]
            else
                found := (c[i] <> a[i]) and (c[i] <> b[i]);

        if found then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
