
# original
remlic = ../roswell/remlic-pddl.ros $*.tsv "(1 :verbose t :class $(1))" "(1 :verbose t :sparsity 0 :class $(1))" $@ 2> $@.err | tee $@.out
# compute the effect without separating each output
remlic1 = ../roswell/remlic-pddl.ros $*.tsv "(1 :verbose t :class $(1))" "(1 :verbose t :sparsity 0 :oneshot t :class $(1))" $@ 2> $@.err | tee $@.out
# pu-learning
remlic2 = ../roswell/remlic-pddl.ros $*.tsv "(1 :verbose t :class $(1) :unlabeled 0.5 :regularizers (PU-noise-regularizer))" "(1 :verbose t :sparsity 0 :oneshot t :class $(1))" $@ 2> $@.err | tee $@.out

all: test-cnf.pddl
all: test-cnf+.pddl
all: test-eqv.pddl
all: test-eqv+.pddl
all: test-pu-cnf.pddl
all: test-pu-cnf+.pddl
all: test-pu-eqv.pddl
all: test-pu-eqv+.pddl
all: test-rf.pddl
all: test-rfpu.pddl

test.tsv: test.ros
	./test.ros 1000 16 3 > $@

%.pretty.json: %.json
	python -m json.tool $*.json > $@

%.json: %.pddl

%-cnf.pddl: %.tsv ../roswell/remlic-pddl.ros
	$(call remlic1,cnf)
	$(MAKE) $(patsubst %.pddl, %.pretty.json, $@)
	rm $(patsubst %.pddl, %.json, $@)

%-cnf+.pddl: %.tsv ../roswell/remlic-pddl.ros
	$(call remlic1,cnf+)
	$(MAKE) $(patsubst %.pddl, %.pretty.json, $@)
	rm $(patsubst %.pddl, %.json, $@)

%-eqv.pddl: %.tsv ../roswell/remlic-pddl.ros
	$(call remlic1,eqv)
	$(MAKE) $(patsubst %.pddl, %.pretty.json, $@)
	rm $(patsubst %.pddl, %.json, $@)

%-eqv+.pddl: %.tsv ../roswell/remlic-pddl.ros
	$(call remlic1,eqv+)
	$(MAKE) $(patsubst %.pddl, %.pretty.json, $@)
	rm $(patsubst %.pddl, %.json, $@)

%-pu-cnf.pddl: %.tsv ../roswell/remlic-pddl.ros
	$(call remlic2,cnf)
	$(MAKE) $(patsubst %.pddl, %.pretty.json, $@)
	rm $(patsubst %.pddl, %.json, $@)

%-pu-cnf+.pddl: %.tsv ../roswell/remlic-pddl.ros
	$(call remlic2,cnf+)
	$(MAKE) $(patsubst %.pddl, %.pretty.json, $@)
	rm $(patsubst %.pddl, %.json, $@)

%-pu-eqv.pddl: %.tsv ../roswell/remlic-pddl.ros
	$(call remlic2,eqv)
	$(MAKE) $(patsubst %.pddl, %.pretty.json, $@)
	rm $(patsubst %.pddl, %.json, $@)

%-pu-eqv+.pddl: %.tsv ../roswell/remlic-pddl.ros
	$(call remlic2,eqv+)
	$(MAKE) $(patsubst %.pddl, %.pretty.json, $@)
	rm $(patsubst %.pddl, %.json, $@)

%-eqv2.pddl: %.tsv ../roswell/remlic-pddl.ros
	$(call remlic1,eqv2)
	$(MAKE) $(patsubst %.pddl, %.pretty.json, $@)
	rm $(patsubst %.pddl, %.json, $@)

%-eqv2+.pddl: %.tsv ../roswell/remlic-pddl.ros
	$(call remlic1,eqv2+)
	$(MAKE) $(patsubst %.pddl, %.pretty.json, $@)
	rm $(patsubst %.pddl, %.json, $@)

%-dnf.pddl: %.tsv ../roswell/remlic-pddl.ros
	$(call remlic1,dnf)
	$(MAKE) $(patsubst %.pddl, %.pretty.json, $@)
	rm $(patsubst %.pddl, %.json, $@)

%-dnf+.pddl: %.tsv ../roswell/remlic-pddl.ros
	$(call remlic1,dnf+)
	$(MAKE) $(patsubst %.pddl, %.pretty.json, $@)
	rm $(patsubst %.pddl, %.json, $@)

%-rf.pddl: %.tsv ../roswell/rf-pddl.ros
	LANG=en_US.utf8 ../roswell/rf-pddl.ros $*.tsv "(:n-tree 10)" "(:n-tree 10)" $@ 2> $@.err | tee $@.out
	$(MAKE) $(patsubst %.pddl, %.pretty.json, $@)
	rm $(patsubst %.pddl, %.json, $@)

%-rfpu.pddl: %.tsv ../roswell/rf-pddl.ros
	LANG=en_US.utf8 ../roswell/rf-pddl.ros $*.tsv "(:n-tree 10 :pu t :fake others1b)" "(:n-tree 10)" $@ 2> $@.err | tee $@.out
	$(MAKE) $(patsubst %.pddl, %.pretty.json, $@)
	rm $(patsubst %.pddl, %.json, $@)

clean:
	-rm test-*
allclean:
	-rm test.tsv test-*
