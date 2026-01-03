program AtCoder_Cards;
var
   i: int8;
   ch: char;
   ans: boolean;
   s, t: array [0 .. 26] of int32;

function atcoder(ch: char): boolean;
begin
  atcoder := (ch = 'a') or (ch = 't') or (ch = 'c') or (ch = 'o') or (ch = 'd') or (ch = 'e') or (ch = 'r');
end;

begin
  for i := 0 to 26 do begin
   s[i] := 0;
   t[i] := 0;
  end;

  repeat
      read(ch);
      inc(s[ord(ch) and 31]);
  until eoln;
  readln;

  repeat
     read(ch);
     inc(t[ord(ch) and 31]);
  until eoln;
  readln;

  for i := 0 to 26 do write(s[i]); writeln;
  for i := 0 to 26 do write(t[i]); writeln;

  ans := true;
  for i := 1 to 26 do begin

      if not atcoder(chr(i-1 + ord('a'))) then
      else if s[i] < t[i] then begin
        dec(s[0], t[i] - s[i]);
        s[i] := t[i];
      end else begin
        dec(t[0], s[i] - t[i]);
        t[i] := s[i];
      end;

      if ans then ans := (s[i] = t[i]);

  end;

  if ans then
     ans := (s[0] >= 0) and (t[0] >= 0);

  if ans then
     writeln('Yes')
  else
      writeln('No');
end.

