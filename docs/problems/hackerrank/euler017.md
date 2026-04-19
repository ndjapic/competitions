# Problem: euler017.pas

```pascal
program Number_to_Words;
const
    ten3 = 1000;
    ten6 = ten3 * ten3;
    ten9 = ten6 * ten3;
    ten12 = int64(ten9) * ten3;
var
    ntc, tci: int8;
    n: int64;

procedure say1(n: int8);
begin
    case n of
        0: write('Zero');
        1: write('One');
        2: write('Two');
        3: write('Three');
        4: write('Four');
        5: write('Five');
        6: write('Six');
        7: write('Seven');
        8: write('Eight');
        9: write('Nine');
        10: write('Ten');
        11: write('Eleven');
        12: write('Twelve');
        13: write('Thir');
        15: write('Fif');
        18: write('Eigh');
        14,16,17,19: say1(n-10);
    end;
    if n > 12 then write('teen');
end;

procedure sayty(n: int8);
begin
    case n of
        2: write('Twen');
        3: write('Thir');
        5: write('Fif');
        8: write('Eigh');
        4,6,7,9: say1(n);
    end;
    write('ty');
end;

procedure saylion(e: int8);
begin
    if e > 0 then write(' ');
    case e of
        0: write('');
        2: write('Hundred');
        3: write('Thousand');
        6: write('M');
        9: write('B');
        12: write('Tr');
    end;
    if e > 3 then write('illion');
end;

procedure say3(n: int16);
begin
    if n >= 100 then begin
        say1(n div 100);
        n := n mod 100;
        saylion(2);
        if n > 0 then write(' ');
    end;

    if n >= 20 then begin
        sayty(n div 10);
        n := n mod 10;
        if n > 0 then write(' ');
    end;

    if n > 0 then say1(n);
end;

procedure three(var n: int64; p: int64; e: int8);
begin
    if n >= p then begin
        say3(n div p);
        n := n mod p;
        saylion(e);
        if n > 0 then write(' ');
    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        if n = 0 then
            say1(0)
        else begin
            three(n, ten12, 12);
            three(n, ten9, 9);
            three(n, ten6, 6);
            three(n, ten3, 3);
            three(n, 1, 0);
        end;
        writeln;

    end;
end.

```
