program A_Shashliks;
uses
	math;
var
    ntc, tci, k, a, b, x, y, ans: int32;

function sol(k, a, b, x, y: int32): int32;
var
	i, j: int32;
begin
	if k < min(a, b) then
		ans := 0
	else if k < b then
		ans := 1 + (k-a) div x
	else if k < a then
		ans := 1 + (k-b) div y
	else begin
		i := 1 + (k-a) div x;
		j := 1 + (k-b) div y;
		ans := max(
			i + sol(k-i*x, a, b, x, y),
			j + sol(k-j*y, a, b, x, y)
		);
	end;
	sol := ans;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(k, a, b, x, y);
        writeln(sol(k, a, b, x, y));

    end;
end.
