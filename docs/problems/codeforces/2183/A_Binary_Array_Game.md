# Problem: A_Binary_Array_Game.pas

```pascal
program A_Binary_Array_Game;
const
	nn = 100;
var
	notc, tci, n, i: int32;
	a: array [1 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]); readln;

		if a[1] = 1 then
			writeln('Alice')
		else if a[n] = 1 then
			writeln('Alice')
		else
			writeln('Bob');

	end;
end.

```
