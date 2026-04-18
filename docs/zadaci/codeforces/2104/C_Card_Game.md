# Задатак: C_Card_Game.pas

```pascal
program C_Card_Game;
{$MODE DELPHI}
var
    ntc, tci: int16;
    n, i: int32;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);
		readln(s);

        if (s[1] = 'A') and (s[n] = 'A') then
            writeln('Alice')
        else if (s[1] = 'B') and (s[n] = 'B') then
            writeln('Bob')
        else if s[1] = 'A' then begin

            i := 1;
            while s[i] = 'A' do inc(i);

            if i = n then
                writeln('Alice')
            else
                writeln('Bob');

        end else if s[n-1] = 'B' then
            writeln('Bob')
        else
            writeln('Alice');

    end;
end.

```
