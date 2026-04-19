# Problem: A_Rearranging_ABC.pas

```pascal
program A_Rearranging_ABC;
{$mode delphi}
var
    s: string;

procedure swp(i, j: int8);
var
    ch: char;
begin
    ch := s[i];
    s[i] := s[j];
    s[j] := ch;
end;

begin
    readln(s);
    if s[2] > s[3] then swp(2, 3);
    if s[1] > s[2] then swp(1, 2);
    if s[2] > s[3] then swp(2, 3);
    if s = 'ABC' then
        writeln('Yes')
    else
        writeln('No');
end.

```
