program C_Tile_Distance_2;
{$mode objfpc}{$H+}{$J-}
uses
    math;
var
    sx, sy, tx, ty, ans: int64;

function d(sx, sy, tx, ty: int64): int64;
begin
    if sy = ty then begin
        result := (tx-sx) div 2;
    end else begin
        result := abs(ty-sy);
        dec(tx, min(tx-sx, result));
        inc(result, d(sx, ty, tx, ty));
    end;
end;

begin
    readln(sx, sy);
    readln(tx, ty);

    if odd(sy) then
        dec(sx, 1 - sx mod 2)
    else
        dec(sx, sx mod 2);

    if odd(ty) then
        dec(tx, 1 - tx mod 2)
    else
        dec(tx, tx mod 2);

    if sx <= tx then
        ans := d(sx, sy, tx, ty)
    else
        ans := d(tx, ty, sx, sy);

    writeln(ans);
end.
