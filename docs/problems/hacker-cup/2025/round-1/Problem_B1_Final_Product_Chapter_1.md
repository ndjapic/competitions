# Problem: Problem_B1_Final_Product_Chapter_1.pas

```pascal
program Problem_B1_Final_Product_Chapter_1;
var
	ntc, tci, n, i, a, b: int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, a, b);

		write('Case #', tci, ': ');
		for i := 1 to 2*n-1 do write('1 ');
		writeln(b);

	end;
end.

```
