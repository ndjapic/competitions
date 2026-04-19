# Problem: B_Above_the_Clouds.pas

```pascal
program B_Above_the_Clouds;
{$MODE DELPHI}
uses
	math;
var
    ntc, tci, n, i: int32;
    x: int8;
    s: string;
    c: array [0 .. 25] of int32;

function o(ch: char): int8;
begin
	o := ord(ch) - ord('a');
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);

		for x := 0 to 25 do c[x] := 0;

		for i := 1 to n do inc(c[o(s[i])]);

		i := 2;
		while (i < n) and (c[o(s[i])] < 2) do inc(i);

		if i < n then
			writeln('Yes')
		else
			writeln('No');

    end;
end.

```
