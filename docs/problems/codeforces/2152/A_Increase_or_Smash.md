# Problem: A_Increase_or_Smash.pas

```pascal
program A_Increase_or_Smash;
uses
	math;
const
	nn = 100;
var
	notc, tci, ans: int16;
	n, i, x: int8;
	seen: array [1 .. nn] of boolean;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for x := 1 to nn do seen[x] := false;

		ans := -1;
		for i := 1 to n do begin
			read(x);
			seen[x] := true;
		end;
		readln;

		for x := 1 to nn do
			if seen[x] then inc(ans, 2);

		writeln(ans);

	end;
end.

```
