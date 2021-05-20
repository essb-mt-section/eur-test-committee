"""
Parser for Remindo JSON result files

Example to convert convert JSON file to csv
    ```
    from remindo_results import RemindoResults
    rr = RemindoResults("remindo_result_exam_A.json",
                            n_questions=41, ignored_questions=[0])
    rr.save_csv_files()
    ```

How to export JSON files for Remindo website? Click on "Results/Analysis". Go to "test moment" and select the test of interest. Click on "Result details". At the end of the page, you can now the option tp export the result details as JSON file.

"""

__author__ = "Oliver Lindemann <lindemann@essb.eur.nl>"
__version__ = "0.3"

import os
import json
import numpy as np
from pandas import DataFrame


class RemindoResults(object):

    def __init__(self, remindo_json_file, n_questions, ignored_questions=[],
                 first_question_label_cnt=0):
        """Parser for Remindo JSON result files

        n_questions:
            n_questions in the data (incl. ignored questions)
        ignore_questions:
                array with question ids to be ignored
        """

        self._n_questions = n_questions
        self._json_file = remindo_json_file
        self._solutions = None
        if not isinstance(ignored_questions, (tuple, list)):
            self._ignored_questions = [ignored_questions]
        else:
            self._ignored_questions = ignored_questions

        with open(remindo_json_file) as fl:
            self.data_dict = json.load(fl)

        self._answer_dict = {
                "student_id":[],
                "grade": [],
                "score": []}

        self.question_labels = ["Q{}".format(x + first_question_label_cnt) \
                                for x in range(n_questions)]
        for x in self.question_labels:
            self._answer_dict[x] = [] # answers to questions

        self._course_students = {}
        for student in self.data_dict:
            course_name = student["recipe_name"]
            if course_name in self._course_students:
                self._course_students[course_name].append(student["user_code"])
            else:
                self._course_students[course_name] = [student["user_code"]]

    @property
    def courses(self):
        """list of courses in the data """
        return list(self._course_students.keys())

    @property
    def course_students(self):
        """student id per course"""
        return self._course_students

    @property
    def students(self):
        return list(self.answers_and_grades["student_id"])

    @property
    def answers_and_grades(self):
        if len(self._answer_dict["student_id"]) == 0:
            #process data dict
            for student in self.data_dict:
                self._answer_dict["student_id"].append(student["user_code"])
                self._answer_dict["grade"].append(student["grade"])
                self._answer_dict["score"].append(student["score"])

                # read in responses in array, because unclear they the come
                # in the correct order (cf "sequence_index")
                answers = [np.NAN] * self._n_questions
                for item in student["sections"][0]["itemresults"]:
                    _id = item["sequence_index"] - 1
                    answers[_id] = _as_number(
                                item["response"]["RESPONSE"]["candidateResponse"])

                # copy response to data structure
                for x, _id in enumerate(self.question_labels):
                    if x not in self._ignored_questions:
                        self._answer_dict[_id].append(answers[x])
                    elif _id in self._answer_dict:
                        # remove from dict questions
                        del self._answer_dict[_id]


        return DataFrame(self._answer_dict)

    @property
    def solutions(self):
        if self._solutions is None:
            # process solutions for each student & check if they are identical
            for student in self.data_dict:
                sol = [np.NAN] * self._n_questions
                for item in student["sections"][0]["itemresults"]:
                    _id = item["sequence_index"] - 1
                    sol[_id]  = _as_number(
                                item["response"]["RESPONSE"]["correctResponse"])

                if self._solutions is None:
                    self._solutions = sol
                elif sol != self._solutions:
                    raise RuntimeError("I can not handle different solutions" +
                                       " for different subjects? ")

            # remove ignored questions
            self._solutions = [x for c, x in enumerate(self._solutions) \
                                        if c not in self._ignored_questions]

        return self._solutions

    def save_csv_files(self, solutions=True, answers=True):

        if solutions:
            fl_name = os.path.splitext(self._json_file)[0]
            # save solutions
            with open(fl_name + ".solutions.csv", "w+") as fl:
                for s in self.solutions:
                    fl.write("{}\n".format(s))

        if answers:
            self.answers_and_grades.to_csv(fl_name + ".answers.csv")


## Helper

def _as_number(letter):
    # convers letter coding to number coding
    if isinstance(letter, list):
        if len(letter) != 1:
            raise RuntimeError("Can't convert response ({})".format(letter) + \
                        "to a single number, because of multiple responses.")
        letter = letter[0]

    return 1 + ord(letter) - ord("A")
