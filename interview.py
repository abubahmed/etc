# def list_head (list1, list2):
#     consecutive_count = defaultdict()
#     list1_index = 0
#     list2_index = 0
#     error_index = None
#     # first find the index of the error
#     while list1_index<len(list1) and list2_index<len(list2):
#         if list1[list1_index] == list2[list2_index]:
#             list1_index += 1
#             list2_index += 1
#         else:
#             error_index = list1_index
#             break
#     if not error_index:
#         return min(len(list1), len(list2))
    
#     # second case, change one index
#     while list1_


class My_class:

    def __init__ (self, window_duration):
        self.values = []
        self.timestamps = []
        self.window_duration = window_duration
    
    def add_data(self, timestamp, value):
        self.values.append(value)
        self.timestamps.append(timestamp)

    def get_mode(self, current_time):
        earliest_time = self.window_duration - current_time
        window_times = []
        for index, timestamp in enumerate(self.timestamps):
            if timestamp > earliest_time:
                window_times.append(self.values[index])
        window_times.sort()

        temp_counter = 1
        champion_count = 1
        mode = window_times[0]
        for i in range(len(window_times)):
            if i == 0:
                continue
            if window_times[i - 1] == window_times[i]:
                temp_counter += 1
                if temp_counter > champion_count:
                    champion_count = temp_counter
                    mode = window_times[i]
            else:
                temp_counter = 1
        return mode
                

                
                


        
