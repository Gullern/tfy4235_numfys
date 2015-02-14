DIRS=num_int testing exercises/ex1 assignments/a1

CLEAN_DIRS=$(addsuffix .clean,$(DIRS))
VCLEAN_DIRS=$(addsuffix .vclean,$(DIRS))

all:

commit:
	git commit
	git push -u origin master

$(DIRS):
	$(MAKE) -C $(basename $@) all

clean: $(CLEAN_DIRS)

vclean: $(VCLEAN_DIRS)

$(CLEAN_DIRS):
	$(MAKE) -C $(basename $@) clean

$(VCLEAN_DIRS):
	$(MAKE) -C $(basename $@) vclean

.PHONY: all clean vclean $(DIRS) $(CLEAN_DIRS) $(VCLEAN_DIRS)

