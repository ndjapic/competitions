# Задатак: C_Make_It_Beautiful.pas

```pascal
program C_Make_It_Beautiful;
const
	nn = 5000;
var
    ntc, tci, n, i: int16;
    beauty: int32;
    k, p2: int64;
    e: int8;
    a: array [1 .. nn] of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);
        for i := 1 to n do read(a[i]); readln;

		p2 := 1;
		beauty := 0;
		for e := 0 to 59 do begin
			for i := 1 to n do
				if a[i] and p2 = p2 then
					inc(beauty)
				else if k >= p2 then begin
					inc(beauty);
					dec(k, p2);
				end;
			inc(p2, p2);
		end;

		writeln(beauty);

    end;
end.

```
