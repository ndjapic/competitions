program Fenwick2D;
{$MODE DELPHI}
const
	N = 1000;
	M = 1000;
// Глобална матрица за Фениково стабло
var
	BIT: array[1 .. N, 1 .. M] of int32;

// Ажурирање: Додаје вредност 'val' на позицију (r, c)
procedure Update2D(r, c, val: int32);
var
	y: int32;
begin
	while r <= N do begin
		y := c;
		while y <= M do begin
			inc(BIT[r, y], val);
			inc(y, y and -y); // Битски трик за кретање кроз Феника
		end;
		inc(r, r and -r);
	end;
end;

// Упит: Враћа суму од (1,1) до (r, c)
function Query2D(r, c: int32): int32;
var
	y: int32;
begin
	result := 0;
	while r > 0 do begin
		y := c;
		while y > 0 do begin
			inc(result, BIT[r, y]);
			dec(y, y and -y);
		end;
		dec(r, r and -r);
	end;
end;

begin
end.
