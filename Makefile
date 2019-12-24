

CFLAGS = -Wall -std=c++17 -Wno-sign-compare -Wno-unused-variable -Wno-shift-count-overflow -Wno-tautological-constant-out-of-range-compare -Wno-mismatched-tags -ftemplate-depth=512 -Wmissing-field-initializers

CC = clang++
LD = clang++

CFLAGS += -Werror -g

OBJDIR = obj

NAME = zenon

ROOT = ./
TOROOT = ./../
IPATH = -I.

CFLAGS += $(IPATH)

LDFLAGS += -L/usr/local/lib

SRCS = $(shell ls -t src/*.cpp)

INSTALL_DIR = $(shell pwd)
CFLAGS += "-DINSTALL_DIR=\"$(INSTALL_DIR)\""

LIBS = -L/usr/lib/x86_64-linux-gnu -lstdc++fs ${LDFLAGS}


OBJS = $(addprefix $(OBJDIR)/,$(SRCS:.cpp=.o))
DEPS = $(addprefix $(OBJDIR)/,$(SRCS:.cpp=.d))
DEPS += $(OBJDIR)/stdafx.h.d

##############################################################################

all:
	@$(MAKE) --no-print-directory info
	@$(MAKE) --no-print-directory compile

compile: $(NAME)

$(OBJDIR)/stdafx.h.gch: src/stdafx.h
	$(CC) -x c++-header $< -MMD $(CFLAGS) -o $@

PCH = $(OBJDIR)/stdafx.h.gch
PCHINC = -include-pch $(OBJDIR)/stdafx.h.gch

$(OBJDIR)/%.o: %.cpp ${PCH}
	$(CC) -MMD $(CFLAGS) $(PCHINC) -c $< -o $@

$(NAME): $(OBJS)
	$(LD) $(CFLAGS) -o $@ $^ $(LIBS)

info:
	@$(CC) -v 2>&1 | head -n 2

clean:
	$(RM) $(OBJDIR)/src/*.o
	$(RM) $(OBJDIR)/src/*.d
	$(RM) $(OBJDIR)/test
	$(RM) $(OBJDIR)-opt/*.o
	$(RM) $(OBJDIR)-opt/*.d
	$(RM) $(NAME)
	$(RM) $(OBJDIR)/stdafx.h.*

-include $(DEPS)
