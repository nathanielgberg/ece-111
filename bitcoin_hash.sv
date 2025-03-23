module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] header_addr, hash_out_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] memory_addr,
                    output logic [31:0] memory_write_data,
                     input logic [31:0] memory_read_data);

parameter num_nonces = 16;

enum logic [3:0] {IDLE,READ, WAIT, COMPUTE_FIRST, COMPUTE_SECOND, PREP_THIRD, COMPUTE_THIRD, WRITE} state;

parameter int k[64] = '{
    32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
    32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
    32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
    32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
    32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
    32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
    32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
    32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};


logic [31:0] message[32];
logic [31:0] first_hash[8];
logic [31:0] second_hash[num_nonces][16];
logic [31:0] hash_out[num_nonces];
logic [31:0] w[16];
logic [31:0] w2[num_nonces][16];
logic [31:0] A2[num_nonces];
logic [31:0] B2[num_nonces];
logic [31:0] C2[num_nonces];
logic [31:0] D2[num_nonces];
logic [31:0] E2[num_nonces];
logic [31:0] F2[num_nonces];
logic [31:0] G2[num_nonces];
logic [31:0] H2[num_nonces];

logic [ 7:0] j;
logic [15:0] present_addr;
logic [31:0] present_write_data;
logic enable_write;
logic [31:0] hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7;
logic [31:0] A, B, C, D, E, F, G, H;
logic [15:0] offset; // in word address
logic read_done;



assign mem_clk = clk;
assign memory_addr = present_addr + offset;
assign mem_we = enable_write;
assign memory_write_data = present_write_data;



// SHA256 hash round
function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                 input logic [7:0] t);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
begin
    S1 = ror(e, 6) ^ ror(e, 11) ^ ror(e, 25);
    ch = (e & f) ^ ((~e) & g);
    t1 = h + S1 + ch + k[t] + w;
    S0 = ror(a, 2) ^ ror(a, 13) ^ ror(a, 22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    t2 = S0 + maj;

    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};

end
endfunction
function logic [31:0] ror(input logic [31:0] in,
                                  input logic [7:0] s);
begin
		ror = (in >> s) | (in << (32-s));
end
endfunction

function logic [31:0] wtnew; // function with no inputs
  logic [31:0] s0, s1;
  s0 = ror(w[1],7)^ror(w[1],18)^(w[1]>>3);
  s1 = ror(w[14],17)^ror(w[14],19)^(w[14]>>10);
  wtnew = w[0] + s0 + w[9] + s1;
endfunction

function logic [31:0] w2tnew(int n); 
  logic [31:0] s0, s1;
  s0 = ror(w2[n][1],7)^ror(w2[n][1],18)^(w2[n][1]>>3);
  s1 = ror(w2[n][14],17)^ror(w2[n][14],19)^(w2[n][14]>>10);
  w2tnew = w2[n][0] + s0 + w2[n][9] + s1;
endfunction

always_ff @(posedge clk, negedge reset_n) begin
  if (!reset_n) begin
		state <= IDLE;
		enable_write <= 0;
  end
  else begin 
	  case (state)
	  	  
	  IDLE: begin 
			if(start) begin 
				 state <= WAIT;
				 present_addr <= header_addr;
				 offset <= 0;
				 enable_write <= 0;
				 
				 hash0 <= 32'h6a09e667;
				 hash1 <= 32'hbb67ae85;
				 hash2 <= 32'h3c6ef372;
				 hash3 <= 32'ha54ff53a;
				 hash4 <= 32'h510e527f;
				 hash5 <= 32'h9b05688c;
				 hash6 <= 32'h1f83d9ab;
				 hash7 <= 32'h5be0cd19;
				 
				 A <= 32'h6a09e667;
				 B <= 32'hbb67ae85;
				 C <= 32'h3c6ef372;
				 D <= 32'ha54ff53a;
				 E <= 32'h510e527f;
				 F <= 32'h9b05688c;
				 G <= 32'h1f83d9ab;
				 H <= 32'h5be0cd19;
				 j <= 0;
				 
			end
	  end
	  
	  READ: begin
			message[offset]<=memory_read_data;
			state <=WAIT;  
			if(offset==19) begin
			  offset<= 0; 
			  read_done <= 1;
			  message[19] <= 32'd0; //nounce 
			  message[20] <= 32'h80000000;
			  message[21] <= 32'd0;
			  message[22] <= 32'd0;
			  message[23] <= 32'd0;
			  message[24] <= 32'd0;
			  message[25] <= 32'd0;
			  message[26] <= 32'd0;
			  message[27] <= 32'd0;
			  message[28] <= 32'd0;
			  message[29] <= 32'd0;
			  message[30] <= 32'd0;
			  message[31] <= 32'd640;

			end else begin
			  offset<= offset + 1;
			  read_done <= 0;
			end
	  end
	  
	  WAIT: begin
			if(read_done) begin
				for(int t = 0; t < 16; t++) begin
						w[t] <= message[t];
				end 

				state <= COMPUTE_FIRST;
			end else begin
				state <= READ;
			end
	  end
	  
	  
	  COMPUTE_FIRST: begin
			if(j < 64) begin 
				{A, B, C, D, E, F, G, H} <= sha256_op(A,B,C,D,E,F,G,H,w[0],j);
				j <= j + 1;
				for (int n = 0; n < 15; n++) begin
					w[n] <= w[n+1]; 
				end
				
				w[15] <= wtnew();
				state<=COMPUTE_FIRST;
			end else begin
				 j <= 0;
				 first_hash[0] <= hash0 + A;
				 first_hash[1] <= hash1 + B;
				 first_hash[2] <= hash2 + C;
				 first_hash[3] <= hash3 + D;
				 first_hash[4] <= hash4 + E;
				 first_hash[5] <= hash5 + F;
				 first_hash[6] <= hash6 + G;
				 first_hash[7] <= hash7 + H;
				 for(int n = 0; n < num_nonces; n++) begin
					A2[n] <= hash0 + A;
					B2[n] <= hash1 + B;
					C2[n] <= hash2 + C;
					D2[n] <= hash3 + D;
					E2[n] <= hash4 + E;
					F2[n] <= hash5 + F;
					G2[n] <= hash6 + G;
					H2[n] <= hash7 + H;
		
					
				 
					for(int i = 0; i < 16; i++) begin
						if(i == 3) begin
							w2[n][i] <= n;
						end else begin
							w2[n][i] <= message[i + 16];
						end
					end
				 end
				 state <= COMPUTE_SECOND;
			end
	  end
	  
	  COMPUTE_SECOND: begin
			for(int n = 0; n < num_nonces; n++) begin
				if(j < 64) begin 
					{A2[n], B2[n], C2[n], D2[n], E2[n], F2[n], G2[n], H2[n]} <= sha256_op(A2[n],B2[n],C2[n],D2[n],E2[n],F2[n],G2[n],H2[n],w2[n][0],j);
					
					for (int i = 0; i < 15; i++) begin
						w2[n][i] <= w2[n][i+1]; 
					end
					
					w2[n][15] <= w2tnew(n);
					
				end else begin
					 second_hash[n][0] <= first_hash[0] + A2[n];
					 second_hash[n][1] <= first_hash[1] + B2[n];
					 second_hash[n][2] <= first_hash[2] + C2[n];
					 second_hash[n][3] <= first_hash[3] + D2[n];
					 second_hash[n][4] <= first_hash[4] + E2[n];
					 second_hash[n][5] <= first_hash[5] + F2[n];
					 second_hash[n][6] <= first_hash[6] + G2[n];
					 second_hash[n][7] <= first_hash[7] + H2[n];
					 second_hash[n][8] <= 32'h80000000;
					 second_hash[n][9] <= 32'h00000000;
					 second_hash[n][10] <= 32'h00000000;
					 second_hash[n][11] <= 32'h00000000;
					 second_hash[n][12] <= 32'h00000000;
					 second_hash[n][13] <= 32'h00000000;
					 second_hash[n][14] <= 32'h00000000;
					 second_hash[n][15] <= 32'd256;
					 A2[n] <= 32'h6a09e667;
					 B2[n] <= 32'hbb67ae85;
					 C2[n] <= 32'h3c6ef372;
					 D2[n] <= 32'ha54ff53a;
					 E2[n] <= 32'h510e527f;
					 F2[n] <= 32'h9b05688c;
					 G2[n] <= 32'h1f83d9ab;
					 H2[n] <= 32'h5be0cd19;
					
				end
			end
			
			if(j < 64) begin
				state <= COMPUTE_SECOND;
				j <= j +1;
			end else begin
				state <= PREP_THIRD;
				j <= 0;
			end	
			
	  end
	  
	  PREP_THIRD: begin
			for(int n = 0; n < num_nonces; n++) begin
				for (int i = 0; i < 16; i++) begin
					w2[n][i] <= second_hash[n][i]; 
				end
			end
			state <= COMPUTE_THIRD;
	  end
	  
	  COMPUTE_THIRD: begin
			for(int n = 0; n < num_nonces; n++) begin
				if(j < 64) begin 
					{A2[n], B2[n], C2[n], D2[n], E2[n], F2[n], G2[n], H2[n]} <= sha256_op(A2[n],B2[n],C2[n],D2[n],E2[n],F2[n],G2[n],H2[n],w2[n][0],j);
					
					for (int i = 0; i < 15; i++) begin
						w2[n][i] <= w2[n][i+1]; 
					end
					
					w2[n][15] <= w2tnew(n);
				end else begin
					hash_out[n] <=  32'h6a09e667 + A2[n];
				end
			end
			
			if(j < 64) begin
				state <= COMPUTE_THIRD;
				j <= j +1;
			end else begin
				state <= WRITE;
				present_addr <= hash_out_addr - 1;
				offset <= 0;
				j <= 0;
			end	
	  end
	  
	  WRITE: begin
			if(offset < num_nonces) begin
				present_write_data <= hash_out[offset];
				enable_write <= 1;
				offset <= offset + 1;
				state <= WRITE;
			end else begin
				state <= IDLE;
				offset <= 0;
				enable_write <= 0;
			end
		end
	  
	  endcase
	end	
end

assign done = (state == IDLE);
endmodule
