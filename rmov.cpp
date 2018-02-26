#include <vector>
#include <iostream>
using std::cin;
using std::cout;
using std::cerr;
using std::vector;
using std::endl;

using cell = bool;
using mem = vector<cell>;

using addr = int;

enum kind : char { k_mov = (char) 0, k_cmov = 1, k_rmov = 2, k_rcmov = 3 };

struct instr {
  kind k;
  addr os[5];
};

constexpr instr mov(addr a, addr b) { return {k_mov, {a, b, 0, 0, 0}}; }
constexpr instr cmov(addr a, addr b, addr c, addr d, addr e) {
  return {k_cmov, {a, b, c, d, e}};
}

constexpr instr rmov(addr a, addr b) { return {k_rmov, {a, b, 0, 0, 0}}; }
constexpr instr rcmov(addr a, addr b, addr c, addr d, addr e) {
  return {k_rcmov, {a, b, c, d, e}};
}

inline void run_instr(instr &ip, mem &mem_in, mem &mem_out) {
  //       0 1 2 3 4
  //   mov x y
  //  cmov a x b y z
  //  rmov x y
  // rcmov a x b y z

  switch (ip.k) {
  case k_mov:
    mem_out[ip.os[1]] = mem_in[ip.os[0]];
    //mem_out[ip.os[0]] = 0;
    return;

  case k_cmov:
    {
      cell t = (mem_out[ip.os[2]] = mem_in[ip.os[0]]);
      mem_out[ip.os[3]] = ~t & mem_in[ip.os[1]];
      mem_out[ip.os[4]] = t & mem_in[ip.os[1]];
      //mem_out[ip.os[0]] = mem_out[ip.os[1]] = 0;
    }
    return;

  case k_rmov:
    mem_out[ip.os[0]] = mem_in[ip.os[1]];
    //mem_out[ip.os[1]] = 0;
    return;

  case k_rcmov:
    {
      cell p = mem_in[ip.os[3]], q = mem_in[ip.os[4]];
      if ((p & q) != 0) { throw 2; }
      mem_out[1] = p | q;
      mem_out[0] = mem_in[2];
      //mem_out[ip.os[2]] = mem_out[ip.os[3]] = mem_out[ip.os[4]] = 0;
    }
    return;

  default:
    throw 1;
  }
}

void print_vb(vector<bool> vb, std::ostream &out = cout) {
  out << '[';
  for (bool b : vb) out << (b ? '1' : '0');
  out << ']';
}

void step(mem &mem_in, mem &mem_out, vector<instr> &instrs) {
  for (instr &ip : instrs) {
    run_instr(ip, mem_in, mem_out);
  }
}

void simulate() {
  mem mem1(12);
  //mem1[0] = 1;
  mem1[1] = 1;
  mem mem2(mem1);
  // 0 1 2
  // 3 4 5
  // 6 7 8
  // 9 10 11
  vector<instr> instrs {
    cmov(0, 1, 2, 3, 4)
    //mov(0, 3), mov(1, 4), mov(2, 5),
    //mov(3, 6), mov(4, 7), mov(5, 8),
    //mov(6, 11), mov(7, 10), mov(8, 9)
  };

  print_vb(mem1);
  cout << endl;
  for (int i = 0; i < 25; i++) {
    mem &mem_in = i % 2 == 0 ? mem1 : mem2;
    mem &mem_out = i % 2 == 0 ? mem2 : mem1;

    for (int j = 0, n = mem_out.size(); j < n; j++) mem_out[j] = 0;
    step(mem_in, mem_out, instrs);
    print_vb(mem_out);
    cout << endl;
  }
}

int main() {
  simulate();
}
