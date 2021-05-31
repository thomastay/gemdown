package me.ttay.parser_combinators;

import java.util.Objects;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class CharStream {
  final int endPos;
  int position = 0;
  int line = 1;
  int lineBegin = 0;
  long stateTag = 0;

  final String s;
  /**
   * The provided file name. If not given, defaults to :temp:
   */
  public final String filename;

  /**
   * Represents the end of stream.
   */
  public final static char endOfStreamChar = '\uFFFF';

  /**
   * The state of the CharStream.
   */
  public record CharStreamState(int position, int line, int lineBegin, long stateTag) {
  }

  public CharStream(String _s) {
    this(_s, ":temp:");
  }

  public CharStream(String _s, String _filename) {
    if (_s == null || _filename == null || _filename.length() == 0)
      throw new IllegalArgumentException();
    s = _s;
    endPos = s.length();
    if (s.length() == 0) {
      position = Integer.MIN_VALUE;
    }
    filename = _filename;
  }

  // -------------- Functions describing the CharStream

  /**
   * @return whether the stream has reached the EOF
   */
  public boolean isAtEof() {
    return position == Integer.MIN_VALUE;
  }

  public CharStreamState getState() {
    return new CharStreamState(position, line, lineBegin, stateTag);
  }

  public void setState(CharStreamState stt) {
    position = stt.position();
    line = stt.line();
    lineBegin = stt.lineBegin();
    stateTag = stt.stateTag();
  }

  // ------------- Primitive, low level stream APIs -------------------------
  // ------------- These methods require manual newline registration --------
  // ------------- To encourage use of the higher level APIs, all methods are
  // suffixed with `Raw`

  // ------------- Mutates the Stream --------------

  /**
   * Skips over the next char in the stream and returns the char. If at EOS,
   * returns the EOS char
   */
  public char readRaw() {
    int pos = position; // read from internal field should always be cached
    if (pos < 0) {
      return endOfStreamChar;
    }
    char c = s.charAt(pos++);
    pos++;
    stateTag++;
    position = (pos == endPos) ? Integer.MIN_VALUE : pos;
    return c;
  }

  /**
   * Reads max `len` chars into a string. If at the EOS, returns empty string.
   * 
   * @see read for the newline version of this
   */
  public String readRaw(int len) {
    int pos = position;
    if (pos < 0) {
      return "";
    }
    String s = peekStrRaw(len);
    pos += s.length();
    position = (pos == endPos) ? Integer.MIN_VALUE : pos;
    stateTag++;
    return s;
  }

  /**
   * Skips a single char, except at the end of the stream, where it does nothing.
   * 
   * @see skip
   */
  public void skipRaw() {
    int pos = position; // read from internal field should always be cached
    if (pos < 0)
      return;
    pos++;
    stateTag++;
    position = (pos == endPos) ? Integer.MIN_VALUE : pos;
  }

  /**
   * Skips the given char if the next character matches.
   * 
   * @return false otherwise, or if EOF. Does not update newlines.
   */
  public boolean skipCharRaw(char ch) {
    int pos = position; // read from internal field should always be cached
    if (pos >= 0 && s.charAt(pos) == ch) {
      // Advance
      skipRaw();
      return true;
    }
    return false;
  }

  /**
   * Skips two chars in succession. Same as matchTwoChars, but mutates the stream.
   * If there isn't a match at both places, does not mutate stream.
   * 
   * @see matchTwoChars
   */
  public boolean skipTwoCharsRaw(char c1, char c2) {
    boolean isMatch = matchTwoCharsRaw(c1, c2);
    if (isMatch) {
      position += 2;
      stateTag++;
      return true;
    }
    return false;
  }

  /**
   * Equivalent to doing: charStream.skip(); ch = charStream.peek(); Optimized as
   * this is a common operation
   */
  public char skipAndPeekRaw() {
    // NOTE: relies on setting the EOS position to < -1
    int pos = position + 1;
    if (pos < 0 || pos == endPos) {
      // ok to be a bit inefficient at EOS, since that doesn't happen often.
      position = (pos == endPos) ? Integer.MIN_VALUE : pos;
      return endOfStreamChar;
    }
    stateTag++;
    position = pos;
    return s.charAt(pos);
  }

  /**
   * Skips the given string if it matches. Note that this method doesn't skip
   * newlines automatically, nor does it register them.
   */
  public boolean skipStrRaw(String needle) {
    // assert
    if (needle == null || needle.length() == 0) {
      throw new IllegalArgumentException("Needle passed in should be a non-zero string");
    }
    // act
    int pos = position;
    if (pos < 0)
      return false;
    if (!s.startsWith(needle, pos)) {
      return false;
    }
    pos = pos + needle.length();
    stateTag++;
    position = (pos >= endPos) ? Integer.MIN_VALUE : pos;
    return true;
  }

  // ------------------------ Non Mutating APIs -----------------------

  /**
   * @return The next char without mutating stream. At the end of the stream, the
   *         static var endOfStreamChar is returned.
   */
  public char peekRaw() {
    int pos = position;
    if (pos < 0) {
      return endOfStreamChar;
    }
    return s.charAt(pos);
  }

  /**
   * Note: there is no non-raw version of this API. Callers are expected to
   * perform string transformation if they want to normalize the newlines.
   * 
   * @return The raw string from the current state to <= len chars.
   */
  public String peekStrRaw(int len) {
    if (len < 0)
      throw new IllegalArgumentException("Len should be positive");
    int pos = position;
    if (pos < 0)
      return "";
    int end = pos + len;
    // overflow, or end is past the end of the string.
    if (end >= endPos) {
      return s.substring(pos);
    }
    return s.substring(pos, end);
  }

  /**
   * @return true if the curr char matches `ch`. Does not mutate stream.
   */
  public boolean matchCharRaw(char ch) {
    return peekRaw() == ch;
  }

  /**
   * @return true if the next two chars matches c1 and c2. If at EOF, returns
   *         false.
   */
  public boolean matchTwoCharsRaw(char c1, char c2) {
    if (c1 == '\r' || c1 == '\n' || c2 == '\r' || c2 == '\n') {
      throw new IllegalArgumentException("Cannot be newline chars");
    }
    int pos = position;
    if (pos < 0) {
      return false;
    }
    char c = s.charAt(pos++);
    if (c != c1 || pos == endPos) {
      return false;
    }
    return s.charAt(pos) == c2;
  }

  /**
   * Applies a regex to the stream. It is highly recommended that you start-anchor
   * your regex (using ^). If the charstream is at EOS, it returns a matcher of an
   * empty string.
   * 
   * @return Matcher represeting the match. Note that the Matcher needs to be
   *         invoked with find();
   */
  public Matcher matchRegexRaw(Pattern patt) {
    int pos = position;
    if (pos < 0) {
      // return garbage matcher
      return patt.matcher("");
    }
    Matcher m = patt.matcher(s);
    return m.region(pos, endPos);
  }

  /**
   * For internal use only. Does not update stateTag.
   */
  private void registerNewlineWithPos(int pos) {
    lineBegin = pos;
    line++;
  }

  /**
   * With the stream pointed at the beginning of the new line, registers a newline
   * in the Stream internal state.
   */
  public void registerNewline() {
    registerNewlineWithPos(position);
    stateTag++;
  }

  // ------------------------------------------------------------------------
  // ------------------------------------------------------------------------
  // --------------------------- NEWLINE METHODS ----------------------------
  // ------------- These methods automatically register newlines ------------
  // ------------------------------------------------------------------------
  // ------------------------------------------------------------------------

  /**
   * @return true if the current char is a newline char. Skips over the three
   *         types of newline characters. If not a newline, does not mutate stream
   *         and returns false.
   */
  public boolean skipNewline() {
    int pos = position; // read from internal field should always be cached
    if (pos < 0) {
      return false;
    }
    char c = s.charAt(pos++); // read and advance
    switch (c) {
      case '\r':
        if (pos == endPos) {
          position = Integer.MIN_VALUE;
          stateTag++;
          return true;
        }
        if (s.charAt(pos) == '\n') {
          pos++;
        }
        // fallthrough
      case '\n':
        registerNewlineWithPos(pos);
        position = (pos == endPos) ? Integer.MIN_VALUE : pos;
        stateTag++;
        return true;
    }
    return false;
  }

  /**
   * Like read, except that if a newline is encountered, it skips it and returns
   * \n
   */
  public char read() {
    int pos = position; // read from internal field should always be cached
    if (pos < 0) {
      return endOfStreamChar;
    }
    stateTag++;
    char c = s.charAt(pos++); // read and advance
    switch (c) {
      case '\r':
        if (pos == endPos) {
          position = Integer.MIN_VALUE;
          return '\n';
        }
        if (s.charAt(pos) == '\n') {
          pos++;
        }
        // fallthrough
      case '\n':
        registerNewlineWithPos(pos);
        position = (pos == endPos) ? Integer.MIN_VALUE : pos;
        return '\n';
      default:
        position = (pos == endPos) ? Integer.MIN_VALUE : pos;
        return c;
    }
  }

  /**
   * Like skipCharRaw, except that if char is '\n', it returns the newline
   * character and registers a newline.
   * 
   * @see skipCharRaw
   * @see skipNewline
   */
  public boolean skipChar(char ch) {
    if (ch == '\r')
      throw new IllegalArgumentException("Char c cannot be a \\r. Only \\n is allowed");
    if (ch == '\n')
      return skipNewline();

    int pos = position; // read from internal field should always be cached
    if (pos < 0) {
      return false;
    }
    char c = s.charAt(pos++); // read and advance
    if (c == ch) {
      position = (pos == endPos) ? Integer.MIN_VALUE : pos;
      stateTag++;
      return true;
    }
    return false;
  }

  /**
   * Just like skip(), except it doesn't perform a read on position and doesn't
   * modify statetag. Also doesn't modify position. Factors out commonality in
   * skip() and skipAndPeek(), used in readTilStr too.
   * 
   * @returns the new position, or MIN_VALUE if reached EOS
   */
  private int unsafeSkipInternal(int pos) {
    // ---------- SKIP -----------------
    char c = s.charAt(pos++); // read and advance
    if (pos == endPos) {
      return Integer.MIN_VALUE;
    }
    switch (c) {
      case '\r':
        if (s.charAt(pos) == '\n') {
          pos++;
        }
        // fallthrough
      case '\n':
        registerNewlineWithPos(pos);
        // fallthrough
    }
    return (pos == endPos) ? Integer.MIN_VALUE : pos;
  }

  /**
   * Like skipRaw, except that it handles newlines too.
   * 
   * @see skipRaw
   */
  public void skip() {
    int pos = position; // read from internal field should always be cached
    if (pos < 0) {
      return;
    }
    stateTag++;
    position = unsafeSkipInternal(pos);
  }

  /**
   * Like skipAndPeekRaw, except that it handles newlines and returns newlines
   * 
   * @see skipAndPeekRaw
   */
  public char skipAndPeek() {
    int pos = position; // read from internal field should always be cached
    if (pos < 0) {
      return endOfStreamChar;
    }
    // ---------- SKIP -----------------
    stateTag++;
    pos = unsafeSkipInternal(pos);
    position = pos; // write to internal field
    if (pos == Integer.MIN_VALUE)
      return endOfStreamChar;
    // ---------- PEEK -----------------
    char c2 = s.charAt(pos);
    switch (c2) {
      case '\r':
      case '\n':
        return '\n';
      default:
        return c2;
    }
  }

  // TODO add a max int, in case lines are very long

  /**
   * Reads until the end of the line.
   * 
   * @return The string til the end of the line. Note: the string does not include
   *         the newline, and the next call to the parser points at the beginning
   *         of the next line. If at EOL, returns ""
   */
  public String readTilEOL() {
    int pos = position; // read from internal field should always be cached
    if (pos < 0) {
      return "";
    }
    stateTag++;
    int startPos = pos;
    while (true) {
      char c = s.charAt(pos++); // read and advance
      switch (c) {
        case '\r':
          if (pos == endPos) {
            position = Integer.MIN_VALUE;
            return s.substring(startPos, pos - 1);
          }
          pos++;
          position = (pos == endPos) ? Integer.MIN_VALUE : pos;
          registerNewlineWithPos(pos);
          return s.substring(startPos, pos - 2);
        case '\n':
          registerNewlineWithPos(pos);
          position = (pos == endPos) ? Integer.MIN_VALUE : pos;
          return s.substring(startPos, pos - 1);
      }
      if (pos == endPos) {
        position = Integer.MIN_VALUE;
        return s.substring(startPos, pos);
      }
    }
  }

  /**
   * Returns the location of the next EOL char (\r, \n, or End of stream.),
   * starting at a given position. Does not mutate stream. Mainly for use in
   * formatting errors.
   * 
   * @return
   */
  public int nextEOLPos(int pos) {
    if (pos < 0 || pos > endPos) {
      return endPos;
    }
    while (pos < endPos) {
      char c = s.charAt(pos);
      switch (c) {
        case '\r':
        case '\n':
          return pos;
      }
      pos++;
    }
    return pos;
  }

  // TODO: skip til EOL

  /**
   * Reads until the two chars c1 and c2 come in succession. Handles newlines.
   * Note that c1 and c2 cannot be return (\\r) characters. This function is
   * useful in writing multiline comment parsers.
   * 
   * @apiNote If EOF is reached and no string is matched, returns ""
   * @return The *raw* string literal, meaning \r\n could be in the read string
   *         too. The reason is for performance, and often the raw string literal
   *         is desired in comments.
   * 
   * @example readTilChars('*', '/');
   */
  public String readTilChars(char c1, char c2) {
    if (c1 == '\r' || c2 == '\r') {
      throw new IllegalArgumentException("Cannot be newline chars");
    }
    int pos = position;
    if (pos < 0) {
      return "";
    }
    int startPos = pos;
    stateTag++;
    while (true) {
      char c = s.charAt(pos++);
      if (pos == endPos) {
        position = Integer.MIN_VALUE;
        return "";
      }
      // Handle newline first
      switch (c) {
        case '\r':
          if (s.charAt(pos) == '\n') {
            pos++;
          }
          // fallthrough to handle \n
        case '\n':
          if (pos == endPos) {
            // no need to register newline, since we are finished.
            position = Integer.MIN_VALUE;
            return "";
          }
          registerNewlineWithPos(pos);
          continue; // loop to handle back-to-back newlines
      }
      if (c != c1) {
        continue;
      }
      if (s.charAt(pos) == c2) {
        position = pos;
        return s.substring(startPos, pos);
      }
    }
  }

  /**
   * Reads until the string `s`, including the string. Note that s cannot contain
   * newline characters, and must be at least 3 characters long (if less, use
   * readTwoChars).
   * 
   * If the string is not found, returns "" and ends the stream. If this is not
   * desired, consider using the non-mutating API findStr
   * 
   * @return The *raw* string literal, meaning \r\n could be in the read string
   *         too. The reason is for performance, and often the raw string literal
   *         is desired. If the needle is not found, returns "".
   * 
   */
  public String readTilStr(String needle) {
    if (needle.indexOf('\r') != -1) {
      throw new IllegalArgumentException("needle cannot contain return chars.");
    }
    if (needle.length() < 3) {
      throw new IllegalArgumentException("Needle should have at least 3 chars");
    }
    int pos = position; // read from internal field should always be cached
    int startPos = pos;
    // we use the builtin indexOf since it is intrinsic-ed to use SIMD
    // https://github.com/openjdk/jdk/blob/f86b70c391c2909676a7d9821b7367269aa85206/src/java.base/share/classes/java/lang/StringUTF16.java#L489
    // also, I don't want to have to write Boyer Moore from scratch...
    int needlePos = s.indexOf(needle, pos);

    // not found. Advance parser to the end, registering newlines.
    if (needlePos == -1) {
      while (pos != Integer.MIN_VALUE) {
        pos = unsafeSkipInternal(pos);
      }
      position = Integer.MIN_VALUE;
      return ""; // not found
    }

    // Found. skip forward, registering line breaks as we go.
    int needleEnd = needlePos + needle.length();
    int stoppingPoint = needleEnd == endPos ? Integer.MIN_VALUE : needleEnd;
    while (pos != stoppingPoint) {
      pos = unsafeSkipInternal(pos);
    }
    position = pos;
    return s.substring(startPos, needleEnd);
  }

  /**
   * Skips over any whitespace determined by Character.isWhiteSpace. Note that
   * this function skips all whitespace before and after a newline.
   * 
   * @return true if any whitespce was skipped.
   */
  public boolean skipWhitespace() {
    int pos = position; // read from internal field should always be cached
    if (pos < 0)
      return false;
    // quick check if pointing to whitespace. If not, we can inc stateTag
    // ahh... the problems of not having defer()
    char c = s.charAt(pos++);
    if (!Character.isWhitespace(c)) {
      return false;
    }
    stateTag++; // inc since we have one whitespace char
    do {
      if (pos == endPos) {
        // no need to register newline, since we are finished.
        position = Integer.MIN_VALUE;
        return true;
      }

      // handle newlines
      switch (c) {
        case '\r':
          if (s.charAt(pos) == '\n') {
            pos++;
          }
          // fallthrough to handle \n
        case '\n':
          if (pos == endPos) {
            // no need to register newline, since we are finished.
            position = Integer.MIN_VALUE;
            return true;
          }
          registerNewlineWithPos(pos);
          break;
      }

      c = s.charAt(pos++);
    } while (Character.isWhitespace(c));

    // At the very end, pos points to two past the newline char.
    // we set position to the char just after the newline char.
    position = pos - 1;
    return true;
  }

  // ------------------------ Non Mutating APIs -----------------------

  /**
   * Same as peekRaw(), except that newlines (\n, \r, \r\n) are treated as a
   * single char and returned as \n
   */
  public char peek() {
    char c = peekRaw();
    switch (c) {
      case '\r':
      case '\n':
        return '\n';
      default:
        return c;
    }
  }

  // --------------------------- BOILER PLATE ------------------------------

  public String getStream() {
    return s;
  }

  public long getStateTag() {
    return stateTag;
  }

  public int getEndPosition() {
    return endPos;
  }

  /**
   * @return A "safe" version of the stream position. If position is set to
   *         INT_MIN, returns endPos
   */
  public int getStreamPosition() {
    return position == Integer.MIN_VALUE ? endPos : position;
  }

  @Override
  public boolean equals(Object o) {
    if (o == this)
      return true;
    if (!(o instanceof CharStream)) {
      return false;
    }
    CharStream charStream = (CharStream) o;
    return Objects.equals(s, charStream.s) && Objects.equals(filename, charStream.filename);
  }

  @Override
  public int hashCode() {
    return Objects.hash(s, filename);
  }

  @Override
  public String toString() {
    return "{" + " s='" + s + "'" + ", filename='" + filename + "'" + "}";
  }

}
