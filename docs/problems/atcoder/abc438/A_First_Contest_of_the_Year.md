# Problem: A_First_Contest_of_the_Year.pas

```pascal
program A_First_Contest_of_the_Year;
var
	d, f: int32;

begin
	readln(d, f);
	d := d mod 7;
	f := (f-1+7-d) mod 7 + 1;
	writeln(f);
end.

```
