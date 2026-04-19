# Problem: A_Secret_Sport.pas

```pascal
program A_Secret_Sport;
const
    maxn = 20;
var
    ntc, tci: int16;
    n, i, x, y: int8;
    aplays, bplays, asets, bsets: int8;
    awon, bwon: boolean;
    s: array [1 .. maxn] of char;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(s[i]); readln;

        awon := false;
        bwon := false;

        for x := 1 to n do begin
            for y := 1 to n div x do begin

                asets := 0;
                bsets := 0;
                aplays := 0;
                bplays := 0;

                i := 1;
                while (i <= n) and (asets < y) and (bsets < y) do begin
                    case s[i] of

                        'A': begin
                            inc(aplays);
                            if aplays = x then begin

                                inc(asets);
                                aplays := 0;
                                bplays := 0;

                            end;
                        end;

                        'B': begin
                            inc(bplays);
                            if bplays = x then begin

                                inc(bsets);
                                aplays := 0;
                                bplays := 0;

                            end;
                        end;

                    end;
                    inc(i);
                end;

                if i > n then begin
                    awon := awon or (asets = y);
                    bwon := bwon or (bsets = y);
                end;

            end;
        end;

        if not bwon then
            writeln('A')
        else if not awon then
            writeln('B')
        else
            writeln('?');

    end;
end.

```
