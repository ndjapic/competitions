# Задатак: B_Make_It_Permutation.pas

```pascal
program B_Make_It_Permutation;
{$INLINE ON}
const
	nn = 5000;
var
    ntc, tci, n, i: int16;

procedure writeop(i, l, r: int16); inline;
begin
	writeln(i, ' ', l, ' ', r);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

		writeln(2*n-1);
		for i := 1 to n do writeop(i, 1, i);
		for i := 1 to n-1 do writeop(i, i+1, n);

    end;
end.

```
